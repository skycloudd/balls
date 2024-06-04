use crate::{
    diagnostics::{Diagnostics, Error},
    parser::ast,
    scopes::Scopes,
};
use balls_span::{MakeSpanned, Spanned};
use chumsky::span::Span as _;
use rustc_hash::FxHashMap;
use typed_ast::{Arg, BinaryOp, Expr, Function, Ident, PostfixOp, TypedAst, TypedExpr, UnaryOp};
use types::{Primitive, Type};

mod typed_ast;
pub mod types;

pub fn typecheck<'src>(
    ast: Spanned<ast::Ast<'src>>,
    diagnostics: &mut Diagnostics<'src>,
) -> Spanned<TypedAst<'src>> {
    Typechecker::new(diagnostics).typecheck(ast)
}

struct Typechecker<'dia, 'src> {
    engine: Engine<'src>,
    diagnostics: &'dia mut Diagnostics<'src>,

    functions: FxHashMap<&'src str, TypeId>,
    variables: Scopes<&'src str, TypeId>,
}

impl<'dia, 'src> Typechecker<'dia, 'src> {
    fn new(diagnostics: &'dia mut Diagnostics<'src>) -> Self {
        Self {
            engine: Engine::new(),
            diagnostics,
            functions: FxHashMap::default(),
            variables: Scopes::new(),
        }
    }

    fn typecheck(&mut self, ast: Spanned<ast::Ast<'src>>) -> Spanned<TypedAst<'src>> {
        ast.map(|ast| {
            let mut typed_ast = TypedAst { functions: vec![] };

            for function in &ast.functions {
                let ty = Type::Function {
                    parameters: function.0.parameters.as_ref().map(|parameters| {
                        parameters
                            .iter()
                            .map(|arg| arg.0.ty.as_ref().map(lower_type))
                            .collect()
                    }),
                    return_ty: function.0.return_ty.as_ref().map(lower_type).boxed(),
                };

                let signature_span = function.0.name.1.union(function.0.return_ty.1);

                let ty = self.engine.insert_type(Spanned(ty, signature_span));

                self.functions.insert(function.0.name.0 .0, ty);
            }

            for function in ast.functions {
                typed_ast.functions.push(self.typecheck_function(function));
            }

            typed_ast
        })
    }

    fn typecheck_function(
        &mut self,
        function: Spanned<ast::Function<'src>>,
    ) -> Spanned<Function<'src>> {
        function.map(|function| {
            self.variables.push_scope();

            for parameter in &function.parameters.0 {
                let ty = self
                    .engine
                    .insert_type(parameter.0.ty.as_ref().map(lower_type));

                self.variables.insert(parameter.0.name.0 .0, ty);
            }

            let name = function.name.as_ref().map(lower_ident);

            let parameters = function.parameters.as_ref().map(|parameters| {
                parameters
                    .iter()
                    .map(|arg| arg.as_ref().map(lower_arg))
                    .collect()
            });

            let return_ty = function.return_ty.as_ref().map(lower_type);

            let body = self.typecheck_expr(function.body);

            let return_ty_id = self.engine.insert_type(return_ty.clone());
            let body_ty_id = self.engine.insert_type(body.0.ty.clone());

            self.engine
                .unify(return_ty_id, body_ty_id)
                .unwrap_or_else(|err| {
                    self.diagnostics.add_error(err);
                });

            self.variables.pop_scope();

            Function {
                name,
                parameters,
                return_ty,
                body,
            }
        })
    }

    #[allow(clippy::too_many_lines)]
    fn typecheck_expr(&mut self, expr: Spanned<ast::Expr<'src>>) -> Spanned<TypedExpr<'src>> {
        expr.map_with_span(|expr, expr_span| match expr {
            ast::Expr::Ident(ident) => {
                let ty = self.variables.get(&ident.0 .0).copied().unwrap_or_else(|| {
                    self.functions.get(ident.0 .0).copied().unwrap_or_else(|| {
                        self.diagnostics.add_error(Error::UndefinedName {
                            ident: ident.0 .0,
                            span: ident.1,
                        });
                        self.engine.insert(Spanned(TypeInfo::Error, ident.1))
                    })
                });

                let ty = self.engine.reconstruct(ty).unwrap_or_else(|err| {
                    self.diagnostics.add_error(err);
                    Spanned(Type::Error, ident.1)
                });

                TypedExpr {
                    ty,
                    expr: Expr::Ident(ident.as_ref().map(lower_ident)),
                }
            }
            ast::Expr::Integer(integer) => TypedExpr {
                ty: Type::Primitive(Primitive::Integer).spanned(expr_span),
                expr: Expr::Integer(integer),
            },
            ast::Expr::Float(float) => TypedExpr {
                ty: Type::Primitive(Primitive::Float).spanned(expr_span),
                expr: Expr::Float(float),
            },
            ast::Expr::Boolean(boolean) => TypedExpr {
                ty: Type::Primitive(Primitive::Boolean).spanned(expr_span),
                expr: Expr::Boolean(boolean),
            },
            ast::Expr::Lazy(expr) => {
                let expr = self.typecheck_expr(expr.unbox());

                TypedExpr {
                    ty: expr.0.ty.clone().map_span(|_| expr_span),
                    expr: Expr::Lazy(expr.boxed()),
                }
            }
            ast::Expr::Binary { op, lhs, rhs } => {
                let lhs = self.typecheck_expr(lhs.unbox());
                let rhs = self.typecheck_expr(rhs.unbox());

                let lhs_ty = self.engine.insert_type(lhs.0.ty.clone());
                let rhs_ty = self.engine.insert_type(rhs.0.ty.clone());

                self.engine.unify(lhs_ty, rhs_ty).unwrap_or_else(|err| {
                    self.diagnostics.add_error(err);
                });

                let expr_ty = {
                    use ast::BinaryOp::{
                        Add, Divide, Equals, GreaterThan, GreaterThanEqual, LessThan,
                        LessThanEqual, Multiply, NotEquals, Subtract,
                    };
                    use Primitive::{Boolean, Float, Integer};
                    use Type::Primitive as Pr;

                    match (&op.0, (&lhs.0.ty.0, &rhs.0.ty.0)) {
                        (_, (Type::Error, _) | (_, Type::Error)) => Type::Error,
                        (Add | Subtract | Multiply | Divide, (Pr(Integer), Pr(Integer))) => {
                            Type::Primitive(Primitive::Integer)
                        }
                        (Add | Subtract | Multiply | Divide, (Pr(Float), Pr(Float))) => {
                            Type::Primitive(Primitive::Float)
                        }
                        (
                            LessThan | GreaterThan | LessThanEqual | GreaterThanEqual | Equals
                            | NotEquals,
                            (Pr(Integer), Pr(Integer)) | (Pr(Float), Pr(Float)),
                        ) => Type::Primitive(Primitive::Boolean),
                        (Equals | NotEquals, (Pr(Boolean), Pr(Boolean))) => {
                            Type::Primitive(Primitive::Boolean)
                        }
                        _ => {
                            self.diagnostics.add_error(Error::CannotBinaryOp {
                                lhs_ty: lhs.0.ty.clone().map_span(|_| lhs.1),
                                rhs_ty: rhs.0.ty.clone().map_span(|_| rhs.1),
                                op,
                                span: expr_span,
                            });

                            Type::Error
                        }
                    }
                };

                TypedExpr {
                    ty: expr_ty.spanned(expr_span),
                    expr: Expr::Binary {
                        op: op.map(lower_binary_op),
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                }
            }
            ast::Expr::Unary { op, expr } => {
                let expr = self.typecheck_expr(expr.unbox());

                let expr_ty = match (op.0, &expr.0.ty.0) {
                    (_, Type::Error) => Type::Error,
                    (ast::UnaryOp::Negate, Type::Primitive(Primitive::Integer)) => {
                        Type::Primitive(Primitive::Integer)
                    }
                    (ast::UnaryOp::Negate, Type::Primitive(Primitive::Float)) => {
                        Type::Primitive(Primitive::Float)
                    }
                    (ast::UnaryOp::Not, Type::Primitive(Primitive::Boolean)) => {
                        Type::Primitive(Primitive::Boolean)
                    }
                    _ => {
                        self.diagnostics.add_error(Error::CannotUnaryOp {
                            expr_ty: expr.0.ty.clone(),
                            op,
                            span: expr_span,
                        });

                        Type::Error
                    }
                };

                TypedExpr {
                    ty: expr_ty.spanned(expr_span),
                    expr: Expr::Unary {
                        op: op.map(lower_unary_op),
                        expr: expr.boxed(),
                    },
                }
            }
            ast::Expr::Postfix { expr, op } => {
                let expr = self.typecheck_expr(expr.unbox());

                match op.0 {
                    ast::PostfixOp::Call(args) => {
                        let args = args.map(|args| {
                            args.into_iter()
                                .map(|arg| self.typecheck_expr(arg))
                                .collect::<Vec<_>>()
                        });

                        let return_ty = match &expr.0.ty.0 {
                            Type::Function {
                                parameters,
                                return_ty,
                            } => {
                                if parameters.0.len() != args.0.len() {
                                    self.diagnostics.add_error(Error::ArgumentCountMismatch {
                                        expected: parameters.0.len(),
                                        found: args.0.len(),
                                        expected_span: parameters.1,
                                        found_span: args.1,
                                    });

                                    return TypedExpr {
                                        ty: return_ty.clone().unbox(),
                                        expr: Expr::Error,
                                    };
                                }

                                for (arg, param) in
                                    args.0.clone().into_iter().zip(parameters.0.clone())
                                {
                                    let arg_ty = self.engine.insert_type(arg.0.ty);

                                    let param_ty = self.engine.insert_type(param);

                                    self.engine.unify(param_ty, arg_ty).unwrap_or_else(|err| {
                                        self.diagnostics.add_error(err);
                                    });
                                }

                                return_ty.clone().unbox()
                            }
                            Type::Error => Type::Error.spanned(expr.0.ty.1),
                            _ => {
                                self.diagnostics.add_error(Error::CannotCall {
                                    ty: expr.0.ty.clone(),
                                    span: expr.1,
                                });

                                Type::Error.spanned(expr.0.ty.1)
                            }
                        };

                        TypedExpr {
                            ty: return_ty,
                            expr: Expr::Postfix {
                                expr: expr.boxed(),
                                op: PostfixOp::Call(args).spanned(op.1),
                            },
                        }
                    }
                    ast::PostfixOp::FieldAccess(field_name) => {
                        self.diagnostics.add_error(Error::FeatureNotImplemented {
                            feature: "field access",
                            span: op.1,
                        });

                        TypedExpr {
                            ty: Type::Error.spanned(op.1),
                            expr: Expr::Postfix {
                                expr: expr.boxed(),
                                op: PostfixOp::FieldAccess(field_name.as_ref().map(lower_ident))
                                    .spanned(op.1),
                            },
                        }
                    }
                }
            }
        })
    }
}

fn lower_type<'src>(ty: &ast::Type<'src>) -> Type<'src> {
    match ty.0 {
        "int" => Type::Primitive(Primitive::Integer),
        "float" => Type::Primitive(Primitive::Float),
        "bool" => Type::Primitive(Primitive::Boolean),
        other => Type::UserDefined(other),
    }
}

const fn lower_ident<'src>(ident: &ast::Ident<'src>) -> Ident<'src> {
    Ident(ident.0)
}

fn lower_arg<'src>(arg: &ast::Arg<'src>) -> Arg<'src> {
    Arg {
        name: arg.name.as_ref().map(lower_ident),
        ty: arg.ty.as_ref().map(lower_type),
    }
}

const fn lower_binary_op(op: ast::BinaryOp) -> BinaryOp {
    match op {
        ast::BinaryOp::Add => BinaryOp::Add,
        ast::BinaryOp::Subtract => BinaryOp::Subtract,
        ast::BinaryOp::Multiply => BinaryOp::Multiply,
        ast::BinaryOp::Divide => BinaryOp::Divide,
        ast::BinaryOp::LessThan => BinaryOp::LessThan,
        ast::BinaryOp::GreaterThan => BinaryOp::GreaterThan,
        ast::BinaryOp::LessThanEqual => BinaryOp::LessThanEqual,
        ast::BinaryOp::GreaterThanEqual => BinaryOp::GreaterThanEqual,
        ast::BinaryOp::Equals => BinaryOp::Equals,
        ast::BinaryOp::NotEquals => BinaryOp::NotEquals,
    }
}

const fn lower_unary_op(op: ast::UnaryOp) -> UnaryOp {
    match op {
        ast::UnaryOp::Negate => UnaryOp::Negate,
        ast::UnaryOp::Not => UnaryOp::Not,
    }
}

struct Engine<'src> {
    id_counter: usize,
    vars: FxHashMap<TypeId, Spanned<TypeInfo<'src>>>,
}

impl<'src> Engine<'src> {
    fn new() -> Self {
        Self {
            id_counter: 0,
            vars: FxHashMap::default(),
        }
    }

    fn insert(&mut self, info: Spanned<TypeInfo<'src>>) -> TypeId {
        self.id_counter += 1;
        let id = TypeId(self.id_counter);
        self.vars.insert(id, info);
        id
    }

    fn insert_type(&mut self, ty: Spanned<Type<'src>>) -> TypeId {
        let info = self.type_to_typeinfo(ty);

        self.insert(info)
    }

    /// Unify two [`TypeId`]s.
    ///
    /// The expected type is `a`, and the found type is `b`.
    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Error<'src>> {
        let var_a = &self.vars[&a];
        let var_b = &self.vars[&b];

        match (&var_a.0, &var_b.0) {
            (TypeInfo::Ref(a), _) => self.unify(*a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, *b),

            (TypeInfo::Unknown, _) => {
                self.vars.insert(a, Spanned(TypeInfo::Ref(b), var_b.1));
                Ok(())
            }
            (_, TypeInfo::Unknown) => {
                self.vars.insert(b, Spanned(TypeInfo::Ref(a), var_a.1));
                Ok(())
            }

            (TypeInfo::Error, _) | (_, TypeInfo::Error) => Ok(()),

            (a, b) if a == b => Ok(()),

            _ => Err(Error::TypeMismatch {
                expected: self.typeinfo_to_type(var_a.clone()),
                found: self.typeinfo_to_type(var_b.clone()),
            }),
        }
    }

    fn reconstruct(&self, id: TypeId) -> Result<Spanned<Type<'src>>, Error<'src>> {
        let var = &self.vars[&id];

        match &var.0 {
            TypeInfo::Error => Ok(Type::Error),
            TypeInfo::Unknown => Err(Error::UnknownType { span: var.1 }),
            TypeInfo::Ref(id) => Ok(self.reconstruct(*id)?.0),
            TypeInfo::Primitive(primitive) => Ok(Type::Primitive(*primitive)),
            TypeInfo::Function {
                parameters,
                return_ty,
            } => Ok(Type::Function {
                parameters: parameters
                    .0
                    .iter()
                    .map(|arg| self.reconstruct(*arg))
                    .collect::<Result<Vec<_>, _>>()?
                    .spanned(parameters.1),
                return_ty: self.reconstruct(*return_ty)?.boxed(),
            }),
            TypeInfo::UserDefined(name) => Ok(Type::UserDefined(name)),
        }
        .map(|ty| ty.spanned(var.1))
    }

    fn type_to_typeinfo(&mut self, ty: Spanned<Type<'src>>) -> Spanned<TypeInfo<'src>> {
        ty.map(|ty| match ty {
            Type::Error => TypeInfo::Error,
            Type::Primitive(primitive) => TypeInfo::Primitive(primitive),
            Type::Function {
                parameters,
                return_ty,
            } => TypeInfo::Function {
                parameters: parameters.map(|parameters| {
                    parameters
                        .into_iter()
                        .map(|arg| self.insert_type(arg))
                        .collect()
                }),
                return_ty: self.insert_type(return_ty.unbox()),
            },
            Type::UserDefined(name) => TypeInfo::UserDefined(name),
        })
    }

    #[allow(clippy::unwrap_used)]
    fn typeinfo_to_type(&self, info: Spanned<TypeInfo<'src>>) -> Spanned<Type<'src>> {
        info.map(|info| match info {
            TypeInfo::Error | TypeInfo::Unknown => Type::Error,
            TypeInfo::Ref(id) => self.reconstruct(id).unwrap().0,
            TypeInfo::Primitive(primitive) => Type::Primitive(primitive),
            TypeInfo::Function {
                parameters,
                return_ty,
            } => Type::Function {
                parameters: parameters.map(|parameters| {
                    parameters
                        .iter()
                        .map(|arg| self.reconstruct(*arg).unwrap())
                        .collect()
                }),
                return_ty: self.reconstruct(return_ty).unwrap().boxed(),
            },
            TypeInfo::UserDefined(name) => Type::UserDefined(name),
        })
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct TypeId(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeInfo<'src> {
    Error,
    #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Primitive(Primitive),
    Function {
        parameters: Spanned<Vec<TypeId>>,
        return_ty: TypeId,
    },
    UserDefined(&'src str),
}

use crate::{
    diagnostics::{Diagnostics, Error},
    join_comma,
    parser::ast,
    scopes::Scopes,
    span::{MakeSpanned as _, Span, Spanned},
    RODEO,
};
use chumsky::span::Span as _;
use lasso::Spur;
use rustc_hash::FxHashMap;
use typed_ast::{
    Arg, BinaryOp, Expr, Function, Ident, MatchArm, Pattern, PostfixOp, TypedAst, TypedExpr,
    UnaryOp,
};
use types::{Primitive, Type};

pub mod typed_ast;
pub mod types;

pub struct Typechecker<'d> {
    engine: Engine,
    diagnostics: &'d mut Diagnostics,

    functions: FxHashMap<Ident, TypeId>,
    variables: Scopes<Ident, TypeId>,
}

impl<'d> Typechecker<'d> {
    pub fn new(diagnostics: &'d mut Diagnostics) -> Self {
        Self {
            engine: Engine::new(),
            diagnostics,
            functions: FxHashMap::default(),
            variables: Scopes::new(),
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn typecheck(&mut self, ast: Spanned<ast::Ast>) -> Spanned<TypedAst> {
        ast.map(|ast| {
            let mut typed_ast = TypedAst { functions: vec![] };

            for function in &ast.functions {
                let ty = Type::Function {
                    parameters: function.0.parameters.as_ref().map(|parameters| {
                        parameters
                            .iter()
                            .map(|arg| arg.0.ty.as_ref().map(Self::lower_type))
                            .collect()
                    }),
                    return_ty: function.0.return_ty.as_ref().map(Self::lower_type).boxed(),
                };

                if function.0.name.0 .0 == "main" {
                    if let Type::Function {
                        parameters,
                        return_ty,
                    } = ty.clone()
                    {
                        if !parameters.0.is_empty() {
                            self.diagnostics
                                .add_error(Error::MainFunctionHasParameters {
                                    span: function.0.parameters.1,
                                });
                        }

                        if return_ty.unbox().0 != Type::Primitive(Primitive::Integer) {
                            self.diagnostics.add_error(Error::MainFunctionReturnType {
                                span: function.0.return_ty.1,
                            });
                        }
                    }
                }

                let signature_span = function.0.name.1.union(function.0.return_ty.1);

                let ty_id = self.engine.insert_type(Spanned(ty, signature_span));

                self.functions
                    .insert(Self::lower_ident(&function.0.name.0), ty_id);
            }

            for function in ast.functions {
                typed_ast.functions.push(self.typecheck_function(function));
            }

            typed_ast
        })
    }

    #[tracing::instrument(name = "typechecking function", skip_all, fields(entity = function.0.name.0 .0))]
    fn typecheck_function(&mut self, function: Spanned<ast::Function>) -> Spanned<Function> {
        function.map(|function| {
            self.variables.push_scope();

            for parameter in &function.parameters.0 {
                let ty = parameter.0.ty.as_ref().map(Self::lower_type);

                let ty = self.engine.insert_type(ty);

                self.variables
                    .insert(Self::lower_ident(&parameter.0.name.0), ty);
            }

            let name = function.name.as_ref().map(Self::lower_ident);

            let parameters = function.parameters.as_ref().map(|parameters| {
                parameters
                    .iter()
                    .map(|arg| arg.as_ref().map(Self::lower_arg))
                    .collect::<Vec<_>>()
            });

            let return_ty = function.return_ty.as_ref().map(Self::lower_type);

            tracing::trace!(
                "function type is ({}) -> {}",
                join_comma(parameters.0.iter().map(|arg| &arg.0.ty.0)),
                return_ty.0
            );

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

    #[tracing::instrument(name = "typechecking expression", skip_all)]
    fn typecheck_expr(&mut self, expr: Spanned<ast::Expr>) -> Spanned<TypedExpr> {
        expr.map_with_span(|expr, expr_span| match expr {
            ast::Expr::Ident(ident) => {
                let ident = ident.as_ref().map(Self::lower_ident);

                let ty = self.variables.get(&ident.0).copied().unwrap_or_else(|| {
                    self.functions.get(&ident.0).copied().unwrap_or_else(|| {
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
                    expr: Expr::Ident(ident),
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
            ast::Expr::Binary { op, lhs, rhs } => {
                let lhs = self.typecheck_expr(lhs.unbox());
                let rhs = self.typecheck_expr(rhs.unbox());

                let lhs_ty = self.engine.insert_type(lhs.0.ty.clone());
                let rhs_ty = self.engine.insert_type(rhs.0.ty.clone());

                let expr_ty = self.bin_op_type(op, lhs.clone(), rhs.clone(), expr_span);

                if expr_ty != Type::Error {
                    self.engine.unify(lhs_ty, rhs_ty).unwrap_or_else(|err| {
                        self.diagnostics.add_error(err);
                    });
                }

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

                let expr_ty = self.unary_op_type(op, expr.clone(), expr_span);

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
                    ast::PostfixOp::Call(args) => self.typecheck_call(expr, args, op.1),
                    ast::PostfixOp::FieldAccess(field_name) => {
                        self.typecheck_field_access(expr, &field_name, op.1)
                    }
                }
            }
            ast::Expr::Match { expr, arms } => self.typecheck_match(expr.unbox(), arms, expr_span),
        })
    }

    fn bin_op_type(
        &mut self,
        op: Spanned<ast::BinaryOp>,
        lhs: Spanned<TypedExpr>,
        rhs: Spanned<TypedExpr>,
        expr_span: Span,
    ) -> Type {
        use ast::BinaryOp::{
            Add, Divide, Equals, GreaterThan, GreaterThanEqual, LessThan, LessThanEqual, Multiply,
            NotEquals, Subtract,
        };
        use Primitive::{Boolean, Float, Integer};
        use Type::Primitive as Pr;

        match (&op.0, (&lhs.0.ty.0, &rhs.0.ty.0)) {
            (_, (Type::Error, _) | (_, Type::Error)) => Type::Error,

            // arithmetic on (int, int) -> int
            (Add | Subtract | Multiply | Divide, (Pr(Integer), Pr(Integer))) => {
                Pr(Primitive::Integer)
            }

            // arithmetic on (float, float) -> float
            (Add | Subtract | Multiply | Divide, (Pr(Float), Pr(Float))) => Pr(Primitive::Float),

            // comparison on (int, int) -> bool
            // and
            // comparison on (float, float) -> bool
            // and
            // comparison on (bool, bool) -> bool
            (
                LessThan | GreaterThan | LessThanEqual | GreaterThanEqual | Equals | NotEquals,
                (Pr(Integer), Pr(Integer)) | (Pr(Float), Pr(Float)),
            )
            | (Equals | NotEquals, (Pr(Boolean), Pr(Boolean))) => Pr(Primitive::Boolean),

            _ => {
                self.diagnostics.add_error(Error::CannotBinaryOp {
                    lhs_ty: lhs.0.ty.map_span(|_| lhs.1),
                    rhs_ty: rhs.0.ty.map_span(|_| rhs.1),
                    op,
                    span: expr_span,
                });

                Type::Error
            }
        }
    }

    fn unary_op_type(
        &mut self,
        op: Spanned<ast::UnaryOp>,
        expr: Spanned<TypedExpr>,
        expr_span: Span,
    ) -> Type {
        match (op.0, &expr.0.ty.0) {
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
                    expr_ty: expr.0.ty,
                    op,
                    span: expr_span,
                });

                Type::Error
            }
        }
    }

    fn typecheck_call(
        &mut self,
        expr: Spanned<TypedExpr>,
        args: Spanned<Vec<Spanned<ast::Expr>>>,
        op_span: Span,
    ) -> TypedExpr {
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

                for (arg, param) in args.0.clone().into_iter().zip(parameters.0.clone()) {
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
                op: PostfixOp::Call(args).spanned(op_span),
            },
        }
    }

    fn typecheck_field_access(
        &mut self,
        expr: Spanned<TypedExpr>,
        field_name: &Spanned<ast::Ident>,
        op_span: Span,
    ) -> TypedExpr {
        self.diagnostics.add_error(Error::FeatureNotImplemented {
            feature: "field access",
            span: op_span,
        });

        TypedExpr {
            ty: Type::Error.spanned(op_span),
            expr: Expr::Postfix {
                expr: expr.boxed(),
                op: PostfixOp::FieldAccess(field_name.as_ref().map(Self::lower_ident))
                    .spanned(op_span),
            },
        }
    }

    fn typecheck_match(
        &mut self,
        expr: Spanned<ast::Expr>,
        arms: Spanned<Vec<Spanned<ast::MatchArm>>>,
        expr_span: Span,
    ) -> TypedExpr {
        let expr = self.typecheck_expr(expr);

        let result_expr_ty = self.engine.insert(TypeInfo::Unknown.spanned(expr_span));

        let result_pattern_ty = self.engine.insert_type(expr.clone().map(|expr| expr.ty.0));

        if arms.0.is_empty() {
            todo!("match expression must have at least one arm");
        }

        let arms = arms.map(|arms| {
            arms.into_iter()
                .map(|arm| {
                    arm.map(|arm| {
                        self.variables.push_scope();

                        if let ast::Pattern::Ident(ident) = &arm.pattern.0 {
                            self.variables.insert(
                                Self::lower_ident(&ident.0),
                                self.engine
                                    .insert_type(expr.0.ty.clone().map_span(|_| arm.expr.1)),
                            );
                        }

                        let arm_expr = self.typecheck_expr(arm.expr);

                        self.variables.pop_scope();

                        let arm_ty = self.engine.insert_type(arm_expr.0.ty.clone());

                        self.engine
                            .unify(result_expr_ty, arm_ty)
                            .unwrap_or_else(|err| {
                                self.diagnostics.add_error(err);
                            });

                        let pattern = self.lower_pattern(arm.pattern, result_pattern_ty);

                        MatchArm {
                            pattern,
                            expr: arm_expr,
                        }
                    })
                })
                .collect()
        });

        let result_ty = self
            .engine
            .reconstruct(result_expr_ty)
            .unwrap_or_else(|err| {
                self.diagnostics.add_error(err);
                Type::Error.spanned(expr_span)
            });

        TypedExpr {
            ty: result_ty,
            expr: Expr::Match {
                expr: expr.boxed(),
                arms,
            },
        }
    }

    fn lower_pattern(
        &mut self,
        pattern: Spanned<ast::Pattern>,
        result_pattern_ty: TypeId,
    ) -> Spanned<Pattern> {
        pattern.map_with_span(|pattern, pat_span| match pattern {
            ast::Pattern::Wildcard => Pattern::Wildcard,
            ast::Pattern::Ident(ident) => Pattern::Ident(ident.as_ref().map(Self::lower_ident)),
            ast::Pattern::Int(value) => {
                let int_ty = self
                    .engine
                    .insert(TypeInfo::Primitive(Primitive::Integer).spanned(pat_span));

                self.engine
                    .unify(result_pattern_ty, int_ty)
                    .unwrap_or_else(|err| {
                        self.diagnostics.add_error(err);
                    });

                Pattern::Int(value)
            }
            ast::Pattern::Float(value) => {
                let float_ty = self
                    .engine
                    .insert(TypeInfo::Primitive(Primitive::Float).spanned(pat_span));

                self.engine
                    .unify(result_pattern_ty, float_ty)
                    .unwrap_or_else(|err| {
                        self.diagnostics.add_error(err);
                    });

                Pattern::Float(value)
            }
            ast::Pattern::Bool(value) => {
                let bool_ty = self
                    .engine
                    .insert(TypeInfo::Primitive(Primitive::Boolean).spanned(pat_span));

                self.engine
                    .unify(result_pattern_ty, bool_ty)
                    .unwrap_or_else(|err| {
                        self.diagnostics.add_error(err);
                    });

                Pattern::Bool(value)
            }
        })
    }

    fn lower_type(ty: &ast::Type) -> Type {
        match ty.0 {
            "int" => Type::Primitive(Primitive::Integer),
            "float" => Type::Primitive(Primitive::Float),
            "bool" => Type::Primitive(Primitive::Boolean),
            other => {
                let key = RODEO.get_or_intern(other);

                Type::UserDefined(key)
            }
        }
    }

    fn lower_ident(ident: &ast::Ident) -> Ident {
        let key = RODEO.get_or_intern(ident.0);

        Ident(key)
    }

    fn lower_arg(arg: &ast::Arg) -> Arg {
        Arg {
            name: arg.name.as_ref().map(Self::lower_ident),
            ty: arg.ty.as_ref().map(Self::lower_type),
        }
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

struct Engine {
    id_counter: usize,
    vars: FxHashMap<TypeId, Spanned<TypeInfo>>,
}

impl Engine {
    fn new() -> Self {
        Self {
            id_counter: 0,
            vars: FxHashMap::default(),
        }
    }

    fn insert(&mut self, info: Spanned<TypeInfo>) -> TypeId {
        self.id_counter += 1;
        let id = TypeId(self.id_counter);
        self.vars.insert(id, info);
        id
    }

    fn insert_type(&mut self, ty: Spanned<Type>) -> TypeId {
        let info = self.type_to_typeinfo(ty);

        self.insert(info)
    }

    /// Unify two [`TypeId`]s.
    ///
    /// The expected type is `a`, and the found type is `b`.
    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Error> {
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
                expected: self.typeinfo_to_type(var_a.clone())?,
                found: self.typeinfo_to_type(var_b.clone())?,
            }),
        }
    }

    fn reconstruct(&self, id: TypeId) -> Result<Spanned<Type>, Error> {
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
            TypeInfo::UserDefined(name) => Ok(Type::UserDefined(*name)),
        }
        .map(|ty| ty.spanned(var.1))
    }

    fn type_to_typeinfo(&mut self, ty: Spanned<Type>) -> Spanned<TypeInfo> {
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

    fn typeinfo_to_type(&self, info: Spanned<TypeInfo>) -> Result<Spanned<Type>, Error> {
        Ok(Spanned(
            match info.0 {
                TypeInfo::Error | TypeInfo::Unknown => Type::Error,
                TypeInfo::Ref(id) => self.reconstruct(id)?.0,
                TypeInfo::Primitive(primitive) => Type::Primitive(primitive),
                TypeInfo::Function {
                    parameters,
                    return_ty,
                } => Type::Function {
                    parameters: Spanned(
                        parameters
                            .0
                            .iter()
                            .map(|arg| self.reconstruct(*arg))
                            .collect::<Result<_, _>>()?,
                        parameters.1,
                    ),
                    return_ty: self.reconstruct(return_ty)?.boxed(),
                },
                TypeInfo::UserDefined(name) => Type::UserDefined(name),
            },
            info.1,
        ))
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct TypeId(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeInfo {
    Error,
    Unknown,
    Ref(TypeId),
    Primitive(Primitive),
    Function {
        parameters: Spanned<Vec<TypeId>>,
        return_ty: TypeId,
    },
    UserDefined(Spur),
}

impl core::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Unknown => write!(f, "unknown"),
            Self::Ref(id) => write!(f, "ref({})", id.0),
            Self::Primitive(primitive) => write!(f, "{primitive}"),
            Self::Function {
                parameters,
                return_ty,
            } => write!(
                f,
                "({parameters}) -> {return_ty}",
                parameters = parameters
                    .0
                    .iter()
                    .map(|id| id.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                return_ty = return_ty.0,
            ),
            Self::UserDefined(name) => write!(f, "{}", RODEO.resolve(name)),
        }
    }
}

use crate::{
    span::Spanned,
    typecheck::typed_ast::{self, Pattern, TypedAst},
    RODEO,
};
use mir::{Arg, BinaryOp, Expr, Function, Ident, MatchArm, Mir, PostfixOp, TypedExpr, UnaryOp};

pub mod mir;

pub struct Lower {}

impl Lower {
    #[tracing::instrument(skip_all)]
    pub fn lower(typed_ast: Spanned<TypedAst>) -> Mir {
        let functions = typed_ast
            .0
            .functions
            .into_iter()
            .map(|function| Self::lower_function(function.0))
            .collect();

        Mir { functions }
    }

    #[tracing::instrument(name = "lowering function", skip_all, fields(entity = RODEO.resolve(&function.name.0 .0)))]
    fn lower_function(function: typed_ast::Function) -> Function {
        let name = function.name.map(Self::lower_ident).0;

        let parameters = function
            .parameters
            .map(|parameters| {
                parameters
                    .into_iter()
                    .map(|param| Self::lower_arg(param.0))
                    .collect()
            })
            .0;

        let return_ty = function.return_ty.0;

        let body = function.body.map(Self::lower_expr).0;

        Function {
            name,
            parameters,
            return_ty,
            body,
        }
    }

    fn lower_expr(expr: typed_ast::TypedExpr) -> TypedExpr {
        TypedExpr {
            ty: expr.ty.0,
            expr: match expr.expr {
                typed_ast::Expr::Error => Expr::Error,
                typed_ast::Expr::Ident(ident) => Expr::Ident(Self::lower_ident(ident.0)),
                typed_ast::Expr::Integer(value) => Expr::Integer(value),
                typed_ast::Expr::Float(value) => Expr::Float(value),
                typed_ast::Expr::Boolean(value) => Expr::Boolean(value),
                typed_ast::Expr::Binary { op, lhs, rhs } => {
                    let lhs = Self::lower_expr(lhs.unbox().0);
                    let rhs = Self::lower_expr(rhs.unbox().0);

                    Expr::Binary {
                        op: Self::lower_binary_op(op.0),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                }
                typed_ast::Expr::Unary { op, expr } => {
                    let expr = Self::lower_expr(expr.unbox().0);

                    Expr::Unary {
                        op: Self::lower_unary_op(op.0),
                        expr: Box::new(expr),
                    }
                }
                typed_ast::Expr::Postfix { expr, op } => {
                    let expr = expr.unbox().map(Self::lower_expr).0;

                    Expr::Postfix {
                        expr: Box::new(expr),
                        op: Self::lower_postfix_op(op.0),
                    }
                }
                typed_ast::Expr::Match {
                    expr: match_expr,
                    arms,
                } => {
                    let match_expr = match_expr.unbox().map(Self::lower_expr).0;

                    let arms = arms
                        .0
                        .into_iter()
                        .map(|arm| {
                            let equal_to =
                                Self::pattern_expr(arm.0.pattern.0).map(|pattern_expr| TypedExpr {
                                    ty: match_expr.ty.clone(),
                                    expr: pattern_expr,
                                });

                            let arm_expr = Self::lower_expr(arm.0.expr.0);

                            MatchArm {
                                equal_to,
                                expr: arm_expr,
                            }
                        })
                        .collect();

                    Expr::Match {
                        expr: Box::new(match_expr),
                        arms,
                    }
                }
            },
        }
    }

    fn pattern_expr(pattern: typed_ast::Pattern) -> Option<Expr> {
        match pattern {
            Pattern::Wildcard => None,
            Pattern::Ident(name) => Some(Expr::Ident(Self::lower_ident(name.0))),
            Pattern::Int(value) => Some(Expr::Integer(value)),
            Pattern::Float(value) => Some(Expr::Float(value)),
            Pattern::Bool(value) => Some(Expr::Boolean(value)),
        }
        .inspect(|expr| tracing::trace!("pattern {:?} => {:?}", pattern, expr))
    }

    fn lower_postfix_op(op: typed_ast::PostfixOp) -> PostfixOp {
        match op {
            typed_ast::PostfixOp::Call(args) => {
                let args = args
                    .0
                    .into_iter()
                    .map(|arg| Self::lower_expr(arg.0))
                    .collect();

                PostfixOp::Call { args }
            }
            typed_ast::PostfixOp::FieldAccess(field) => {
                PostfixOp::FieldAccess(Self::lower_ident(field.0))
            }
        }
    }

    const fn lower_ident(ident: typed_ast::Ident) -> Ident {
        Ident(ident.0)
    }

    fn lower_arg(arg: typed_ast::Arg) -> Arg {
        Arg {
            name: Self::lower_ident(arg.name.0),
            ty: arg.ty.0,
        }
    }

    const fn lower_binary_op(op: typed_ast::BinaryOp) -> BinaryOp {
        match op {
            typed_ast::BinaryOp::Add => BinaryOp::Add,
            typed_ast::BinaryOp::Subtract => BinaryOp::Subtract,
            typed_ast::BinaryOp::Multiply => BinaryOp::Multiply,
            typed_ast::BinaryOp::Divide => BinaryOp::Divide,
            typed_ast::BinaryOp::LessThan => BinaryOp::LessThan,
            typed_ast::BinaryOp::GreaterThan => BinaryOp::GreaterThan,
            typed_ast::BinaryOp::LessThanEqual => BinaryOp::LessThanEqual,
            typed_ast::BinaryOp::GreaterThanEqual => BinaryOp::GreaterThanEqual,
            typed_ast::BinaryOp::Equals => BinaryOp::Equals,
            typed_ast::BinaryOp::NotEquals => BinaryOp::NotEquals,
        }
    }

    const fn lower_unary_op(op: typed_ast::UnaryOp) -> UnaryOp {
        match op {
            typed_ast::UnaryOp::Negate => UnaryOp::Negate,
            typed_ast::UnaryOp::Not => UnaryOp::Not,
        }
    }
}

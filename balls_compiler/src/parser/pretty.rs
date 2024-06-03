use super::ast::{Ast, Expr, Function, PostfixOp};
use balls_span::Spanned;
use chumsky::span::Span as _;
use ptree::TreeItem;
use std::borrow::Cow;

impl TreeItem for Ast {
    type Child = WithSpan<Function>;

    fn write_self<W: std::io::Write>(
        &self,
        f: &mut W,
        style: &ptree::Style,
    ) -> std::io::Result<()> {
        write!(f, "{}", style.paint("Ast"))
    }

    fn children(&self) -> std::borrow::Cow<[Self::Child]> {
        Cow::Owned(
            self.functions
                .clone()
                .into_iter()
                .map(Into::into)
                .collect::<Vec<_>>(),
        )
    }
}

impl TreeItem for WithSpan<Function> {
    type Child = WithSpan<Expr>;

    fn write_self<W: std::io::Write>(
        &self,
        f: &mut W,
        style: &ptree::Style,
    ) -> std::io::Result<()> {
        write!(
            f,
            "{} ({}) :: {}",
            style.paint(self.0.name.0 .0),
            style.paint(
                self.0
                    .parameters
                    .0
                    .iter()
                    .map(|param| format!("{} :: {}", param.0.name.0 .0, param.0.ty.0 .0))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            style.paint(self.0.return_ty.0 .0)
        )
    }

    fn children(&self) -> Cow<[Self::Child]> {
        Cow::Owned(vec![self.0.body.clone().into()])
    }
}

impl TreeItem for WithSpan<Expr> {
    type Child = Self;

    fn write_self<W: std::io::Write>(
        &self,
        f: &mut W,
        style: &ptree::Style,
    ) -> std::io::Result<()> {
        write!(
            f,
            "{} @ {}",
            style.paint(match &self.0 {
                Expr::Ident(name) => format!("Ident<{}>", name.0 .0),
                Expr::Integer(integer) => format!("Integer<{integer}>"),
                Expr::Float(float) => format!("Float<{float}>"),
                Expr::Boolean(boolean) => format!("Boolean<{boolean}>"),
                Expr::Lazy(_) => "Lazy".to_string(),
                Expr::Binary { op, lhs: _, rhs: _ } => op.0.to_string(),
                Expr::Unary { op, expr: _ } => op.0.to_string(),
                Expr::Postfix { expr: _, op } => op.0.to_string(),
            }),
            style.paint(format!("{}..{}", self.1.start(), self.1.end()))
        )
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match self.0.clone() {
            Expr::Ident(_) | Expr::Integer(_) | Expr::Float(_) | Expr::Boolean(_) => {
                Cow::Borrowed(&[])
            }
            Expr::Lazy(expr) | Expr::Unary { op: _, expr } => Cow::Owned(vec![expr.unbox().into()]),
            Expr::Postfix { expr, op } => {
                let mut children = vec![expr.unbox().into()];

                match op.0 {
                    PostfixOp::Call(args) => {
                        children.extend(args.0.into_iter().map(Into::into));
                    }
                    PostfixOp::FieldAccess(_) => {}
                }

                Cow::Owned(children)
            }
            Expr::Binary { op: _, lhs, rhs } => {
                Cow::Owned(vec![lhs.unbox().into(), rhs.unbox().into()])
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct WithSpan<T>(T, balls_span::Span);

impl<T> From<Spanned<T>> for WithSpan<T> {
    fn from(spanned: Spanned<T>) -> Self {
        Self(spanned.0, spanned.1)
    }
}

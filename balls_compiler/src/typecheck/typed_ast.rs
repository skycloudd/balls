use super::types::Type;
use crate::{
    lexer::token::{FloatTy, IntTy},
    span::Spanned,
};
use lasso::Spur;

#[derive(Clone, Debug)]
pub struct TypedAst {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Ident(pub Spur);

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Ident>,
    pub parameters: Spanned<Vec<Spanned<Arg>>>,
    pub return_ty: Spanned<Type>,
    pub body: Spanned<TypedExpr>,
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub name: Spanned<Ident>,
    pub ty: Spanned<Type>,
}

#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub ty: Spanned<Type>,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Error,
    Ident(Spanned<Ident>),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Binary {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<TypedExpr>>,
        rhs: Spanned<Box<TypedExpr>>,
    },
    Unary {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<TypedExpr>>,
    },
    Postfix {
        expr: Spanned<Box<TypedExpr>>,
        op: Spanned<PostfixOp>,
    },
    Match {
        expr: Spanned<Box<TypedExpr>>,
        arms: Spanned<Vec<Spanned<MatchArm>>>,
    },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub expr: Spanned<TypedExpr>,
}

#[derive(Clone, Copy, Debug)]
pub enum Pattern {
    Wildcard,
    Ident(Spanned<Ident>),
    Int(IntTy),
    Float(FloatTy),
    Bool(bool),
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,

    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,

    Equals,
    NotEquals,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Clone, Debug)]
pub enum PostfixOp {
    Call(Spanned<Vec<Spanned<TypedExpr>>>),
    FieldAccess(Spanned<Ident>),
}

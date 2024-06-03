use super::types::Type;
use balls_bytecode::{FloatTy, IntTy};
use balls_span::Spanned;

#[derive(Clone, Debug)]
pub struct TypedAst {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Clone, Debug)]
pub struct Ident(pub &'static str);

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
    Lazy(Spanned<Box<TypedExpr>>),
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
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Clone, Debug)]
pub enum PostfixOp {
    Call(Spanned<Vec<Spanned<TypedExpr>>>),
    FieldAccess(Spanned<Ident>),
}

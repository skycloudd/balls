use super::types::Type;
use balls_bytecode::{FloatTy, IntTy};
use balls_span::Spanned;

#[derive(Clone, Debug)]
pub struct TypedAst<'src> {
    pub functions: Vec<Spanned<Function<'src>>>,
}

#[derive(Clone, Debug)]
pub struct Ident<'src>(pub &'src str);

#[derive(Clone, Debug)]
pub struct Function<'src> {
    pub name: Spanned<Ident<'src>>,
    pub parameters: Spanned<Vec<Spanned<Arg<'src>>>>,
    pub return_ty: Spanned<Type<'src>>,
    pub body: Spanned<TypedExpr<'src>>,
}

#[derive(Clone, Debug)]
pub struct Arg<'src> {
    pub name: Spanned<Ident<'src>>,
    pub ty: Spanned<Type<'src>>,
}

#[derive(Clone, Debug)]
pub struct TypedExpr<'src> {
    pub ty: Spanned<Type<'src>>,
    pub expr: Expr<'src>,
}

#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Error,
    Ident(Spanned<Ident<'src>>),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Lazy(Spanned<Box<TypedExpr<'src>>>),
    Binary {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<TypedExpr<'src>>>,
        rhs: Spanned<Box<TypedExpr<'src>>>,
    },
    Unary {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<TypedExpr<'src>>>,
    },
    Postfix {
        expr: Spanned<Box<TypedExpr<'src>>>,
        op: Spanned<PostfixOp<'src>>,
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
pub enum PostfixOp<'src> {
    Call(Spanned<Vec<Spanned<TypedExpr<'src>>>>),
    FieldAccess(Spanned<Ident<'src>>),
}

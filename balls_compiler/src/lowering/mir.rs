use crate::lexer::token::{FloatTy, IntTy};
use crate::typecheck::types::Type;
use lasso::Spur;

#[derive(Clone, Debug)]
pub struct Mir {
    pub functions: Vec<Function>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Ident(pub Spur);

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Ident,
    pub parameters: Vec<Arg>,
    pub return_ty: Type,
    pub body: TypedExpr,
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub ty: Type,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Error,
    Ident(Ident),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Binary {
        op: BinaryOp,
        lhs: Box<TypedExpr>,
        rhs: Box<TypedExpr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<TypedExpr>,
    },
    Postfix {
        expr: Box<TypedExpr>,
        op: PostfixOp,
    },
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
    Call { args: Vec<TypedExpr> },
    FieldAccess(Ident),
}

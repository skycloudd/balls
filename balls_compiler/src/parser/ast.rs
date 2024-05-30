use balls_bytecode::{FloatTy, IntTy};
use balls_span::Spanned;

#[derive(Clone, Debug)]
pub struct Ast {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Clone, Debug)]
pub struct Ident(pub &'static str);

#[derive(Clone, Debug)]
pub struct Type(pub &'static str);

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Ident>,
    pub args: Spanned<Vec<Spanned<Arg>>>,
    pub return_ty: Spanned<Type>,
    pub body: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub name: Spanned<Ident>,
    pub ty: Spanned<Type>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Ident(Spanned<Ident>),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Lazy(Spanned<Box<Expr>>),
    Binary {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<Expr>>,
        rhs: Spanned<Box<Expr>>,
    },
    Unary {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<Expr>>,
    },
    Postfix {
        expr: Spanned<Box<Expr>>,
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
    Call(Spanned<Vec<Spanned<Expr>>>),
    FieldAccess(Spanned<Ident>),
}

use balls_bytecode::{FloatTy, IntTy};
use balls_span::Spanned;

#[derive(Clone, Debug)]
pub struct Ast<'src> {
    pub functions: Vec<Spanned<Function<'src>>>,
}

#[derive(Clone, Debug)]
pub struct Ident<'src>(pub &'src str);

#[derive(Clone, Debug)]
pub struct Type<'src>(pub &'src str);

#[derive(Clone, Debug)]
pub struct Function<'src> {
    pub name: Spanned<Ident<'src>>,
    pub parameters: Spanned<Vec<Spanned<Arg<'src>>>>,
    pub return_ty: Spanned<Type<'src>>,
    pub body: Spanned<Expr<'src>>,
}

#[derive(Clone, Debug)]
pub struct Arg<'src> {
    pub name: Spanned<Ident<'src>>,
    pub ty: Spanned<Type<'src>>,
}

#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Ident(Spanned<Ident<'src>>),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Lazy(Spanned<Box<Expr<'src>>>),
    Binary {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<Expr<'src>>>,
        rhs: Spanned<Box<Expr<'src>>>,
    },
    Unary {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<Expr<'src>>>,
    },
    Postfix {
        expr: Spanned<Box<Expr<'src>>>,
        op: Spanned<PostfixOp<'src>>,
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
pub enum PostfixOp<'src> {
    Call(Spanned<Vec<Spanned<Expr<'src>>>>),
    FieldAccess(Spanned<Ident<'src>>),
}

impl core::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThanEqual => write!(f, "<="),
            Self::GreaterThanEqual => write!(f, ">="),
            Self::Equals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),
        }
    }
}

impl core::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Negate => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

impl core::fmt::Display for PostfixOp<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Call(args) => write!(
                f,
                "({})",
                args.0
                    .iter()
                    .map(|_| "_".to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::FieldAccess(ident) => write!(f, ".{}", ident.0 .0),
        }
    }
}

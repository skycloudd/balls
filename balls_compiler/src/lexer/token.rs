use balls_bytecode::{FloatTy, IntTy};
use balls_span::Span;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Tokens<'src>(pub Vec<(Token<'src>, Span)>);

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Error,
    Simple(Simple<'src>),
    Parentheses(Tokens<'src>),
    CurlyBraces(Tokens<'src>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Simple<'src> {
    Ident(&'src str),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kw {
    Let,
    Match,
    With,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Punc {
    DoubleColon,
    DoubleEquals,
    NotEquals,
    SingleArrow,

    Equals,
    Period,
    Comma,
    Exclamation,
    Pipe,

    // arithmetic op
    Plus,
    Minus,
    Star,
    Slash,

    // comparison op
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
}

impl core::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Error => write!(f, "<error>"),
            Self::Simple(simple) => write!(f, "{simple}"),
            Self::Parentheses(tokens) => {
                write!(
                    f,
                    "({})",
                    tokens
                        .0
                        .iter()
                        .map(|(token, _)| token.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Self::CurlyBraces(tokens) => write!(
                f,
                "{{{}}}",
                tokens
                    .0
                    .iter()
                    .map(|(token, _)| token.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

impl core::fmt::Display for Simple<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Integer(integer) => write!(f, "{integer}"),
            Self::Float(float) => write!(f, "{float}"),
            Self::Boolean(boolean) => write!(f, "{boolean}"),
            Self::Kw(kw) => write!(f, "{kw}"),
            Self::Punc(punc) => write!(f, "{punc}"),
        }
    }
}

impl core::fmt::Display for Kw {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Let => write!(f, "let"),
            Self::Match => write!(f, "match"),
            Self::With => write!(f, "with"),
        }
    }
}

impl core::fmt::Display for Punc {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::DoubleColon => write!(f, "::"),
            Self::DoubleEquals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),
            Self::SingleArrow => write!(f, "->"),

            Self::Equals => write!(f, "="),
            Self::Period => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::Exclamation => write!(f, "!"),
            Self::Pipe => write!(f, "|"),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),

            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThanEqual => write!(f, "<="),
            Self::GreaterThanEqual => write!(f, ">="),
        }
    }
}

use balls_bytecode::{FloatTy, IntTy};
use balls_span::Span;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Tokens(pub Vec<(Token, Span)>);

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Error,
    Simple(Simple),
    Parentheses(Tokens),
    CurlyBraces(Tokens),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Simple {
    Ident(&'static str),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kw {
    Let,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Punc {
    DoubleColon,
    DoubleEquals,
    NotEquals,

    Equals,
    Period,
    Comma,
    Exclamation,

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

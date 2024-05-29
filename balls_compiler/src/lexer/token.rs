use balls_bytecode::{FloatTy, IntTy};
use balls_span::Spanned;

#[derive(Clone, Debug)]
pub struct Tokens(pub Vec<Spanned<Token>>);

#[derive(Clone, Debug)]
pub enum Token {
    Simple(Simple),
    Parentheses(Tokens),
}

#[derive(Clone, Debug)]
pub enum Simple {
    Ident(&'static str),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Debug)]
pub enum Kw {
    Let,
}

#[derive(Clone, Debug)]
pub enum Punc {
    DoubleColon,

    Equals,
    Period,
    Comma,

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

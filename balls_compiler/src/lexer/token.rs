use balls_bytecode::{FloatTy, IntTy};
use balls_span::Spanned;
use chumsky::span::Span;
use ptree::TreeItem;
use std::borrow::Cow;

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

impl TreeItem for Tokens {
    type Child = WithSpan<Token>;

    fn write_self<W: std::io::Write>(
        &self,
        f: &mut W,
        style: &ptree::Style,
    ) -> std::io::Result<()> {
        write!(f, "{}", style.paint("Tokens"))
    }

    fn children<'a>(&self) -> Cow<[Self::Child]> {
        Cow::Owned(self.0.iter().map(|t| WithSpan(t.0.clone(), t.1)).collect())
    }
}

impl TreeItem for WithSpan<Token> {
    type Child = Self;

    fn write_self<W: std::io::Write>(
        &self,
        f: &mut W,
        style: &ptree::Style,
    ) -> std::io::Result<()> {
        match &self.0 {
            Token::Simple(s) => {
                write!(f, "{} ", style.paint(s))?;
            }
            Token::Parentheses(_) => {
                write!(f, "{} ", style.paint("(...)"))?;
            }
        }

        write!(
            f,
            "@ {}",
            style.paint(format!("{}..{}", self.1.start(), self.1.end()))
        )
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match &self.0 {
            Token::Simple(_) => Cow::default(),
            Token::Parentheses(tokens) => Cow::Owned(
                tokens
                    .0
                    .iter()
                    .map(|t| Self(t.0.clone(), t.1))
                    .collect::<Vec<_>>(),
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct WithSpan<T>(T, balls_span::Span);

impl core::fmt::Display for Simple {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Ident(v) => write!(f, "Ident<{v}>"),
            Self::Integer(v) => write!(f, "Integer<{v}>"),
            Self::Float(v) => write!(f, "Float<{v}>"),
            Self::Boolean(v) => write!(f, "Boolean<{v}>"),
            Self::Kw(v) => write!(f, "{v}"),
            Self::Punc(v) => write!(f, "{v}"),
        }
    }
}

impl core::fmt::Display for Kw {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Let => write!(f, "let"),
        }
    }
}

impl core::fmt::Display for Punc {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::DoubleColon => write!(f, "::"),
            Self::Equals => write!(f, "="),
            Self::Period => write!(f, "."),
            Self::Comma => write!(f, ","),

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

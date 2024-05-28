use balls_bytecode::{FloatTy, IntTy};
use balls_span::Span;
use chumsky::{input::WithContext, prelude::*};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Error,
    Variable(&'static str),
    Integer(IntTy),
    Float(FloatTy),
    Boolean(bool),
    Kw(Kw),
    Ctrl(Ctrl),
    Op(Op),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kw {
    Return,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ctrl {
    LeftParen,
    RightParen,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
}

impl core::fmt::Display for Token {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            // Self::Error => write!(f, "<error>"),
            Self::Variable(v) => write!(f, "{v}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Integer(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Kw(k) => write!(f, "{k}"),
            Self::Ctrl(c) => write!(f, "{c}"),
            Self::Op(o) => write!(f, "{o}"),
        }
    }
}

impl core::fmt::Display for Kw {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Return => write!(f, "return"),
        }
    }
}

impl core::fmt::Display for Ctrl {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
        }
    }
}

impl core::fmt::Display for Op {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
        }
    }
}

type LexerInput = WithContext<Span, &'static str>;

type LexerOutput = Vec<(Token, Span)>;

type LexerError = extra::Err<Rich<'static, char, Span>>;

pub fn lexer() -> impl Parser<'static, LexerInput, LexerOutput, LexerError> {
    let variable = text::ascii::ident().map(Token::Variable).boxed();

    let bool = choice((
        text::keyword("true").to(Token::Boolean(true)),
        text::keyword("false").to(Token::Boolean(false)),
    ))
    .boxed();

    let sign = choice((just('+'), just('-'))).or_not().boxed();

    let integer = sign
        .clone()
        .then(text::int(10))
        .to_slice()
        .validate(|n: &str, e, emitter| match n.parse() {
            Ok(n) => n,
            Err(err) => {
                emitter.emit(Rich::custom(e.span(), err));
                0
            }
        })
        .map(Token::Integer)
        .boxed();

    let float = sign
        .then(text::int(10))
        .then_ignore(just('.'))
        .then(text::digits(10))
        .to_slice()
        .validate(|n: &str, e, emitter| match n.parse() {
            Ok(n) => n,
            Err(err) => {
                emitter.emit(Rich::custom(e.span(), err));
                0.0
            }
        })
        .map(Token::Float)
        .boxed();

    let keyword = choice((text::keyword("return").to(Token::Kw(Kw::Return)),)).boxed();

    let ctrl = choice((
        just('(').to(Token::Ctrl(Ctrl::LeftParen)),
        just(')').to(Token::Ctrl(Ctrl::RightParen)),
    ))
    .boxed();

    let operator = choice((
        just('+').to(Token::Op(Op::Plus)),
        just('-').to(Token::Op(Op::Minus)),
        just('*').to(Token::Op(Op::Star)),
        just('/').to(Token::Op(Op::Slash)),
    ))
    .boxed();

    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .boxed();

    let token = choice((keyword, bool, variable, float, integer, operator, ctrl))
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.clone().repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .boxed();

    token
        .repeated()
        .collect()
        .padded_by(comment.repeated())
        .padded()
        .then_ignore(end())
        .boxed()
}

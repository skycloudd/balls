use balls_span::{Span, Spanned};
use chumsky::{input::WithContext, prelude::*};
use token::{Kw, Punc, Simple, Token, Tokens};

mod token;

pub fn lexer(
) -> impl Parser<'static, WithContext<Span, &'static str>, Tokens, extra::Err<Rich<'static, char, Span>>>
{
    recursive(|tokens| {
        let ident = text::ascii::ident().map(Simple::Ident).boxed();

        let bool = choice((
            text::keyword("true").to(Simple::Boolean(true)),
            text::keyword("false").to(Simple::Boolean(false)),
        ))
        .boxed();

        let sign = choice((just('+'), just('-'))).or_not().boxed();

        let integer = sign
            .clone()
            .then(text::int(10))
            .to_slice()
            .validate(|n: &str, err, emitter| match n.parse() {
                Ok(n) => n,
                Err(parse_error) => {
                    emitter.emit(Rich::custom(err.span(), parse_error));
                    0
                }
            })
            .map(Simple::Integer)
            .boxed();

        let float = sign
            .then(text::int(10))
            .then_ignore(just('.'))
            .then(text::digits(10))
            .to_slice()
            .validate(|n: &str, err, emitter| match n.parse() {
                Ok(n) => n,
                Err(parse_error) => {
                    emitter.emit(Rich::custom(err.span(), parse_error));
                    0.0
                }
            })
            .map(Simple::Float)
            .boxed();

        let keyword = choice((text::keyword("let").to(Simple::Kw(Kw::Let)),)).boxed();

        let punctuation = choice((
            // 2 char
            //
            just("::").to(Simple::Punc(Punc::DoubleColon)),
            // comparison op
            just("<=").to(Simple::Punc(Punc::LessThanEqual)),
            just(">=").to(Simple::Punc(Punc::GreaterThanEqual)),
            // 1 char
            //
            just("=").to(Simple::Punc(Punc::Equals)),
            just('.').to(Simple::Punc(Punc::Period)),
            just(',').to(Simple::Punc(Punc::Comma)),
            // arithmetic op
            just('+').to(Simple::Punc(Punc::Plus)),
            just('-').to(Simple::Punc(Punc::Minus)),
            just('*').to(Simple::Punc(Punc::Star)),
            just('/').to(Simple::Punc(Punc::Slash)),
            // comparison op
            just('<').to(Simple::Punc(Punc::LessThan)),
            just('>').to(Simple::Punc(Punc::GreaterThan)),
        ))
        .boxed();

        let comment = just("#")
            .then(any().and_is(just('\n').not()).repeated())
            .padded()
            .boxed();

        let simple = choice((keyword, bool, ident, float, integer, punctuation)).map(Token::Simple);

        let parenthesised = tokens
            .delimited_by(just('('), just(')'))
            .recover_with(via_parser(nested_delimiters('(', ')', [], |span| {
                Tokens(vec![Spanned(Token::Error, span)])
            })))
            .map(Token::Parentheses)
            .boxed();

        let token = choice((simple, parenthesised))
            .map_with(|tok, err| Spanned(tok, err.span()))
            .padded_by(comment.clone().repeated())
            .padded()
            .boxed();

        token.repeated().collect().padded().map(Tokens).boxed()
    })
    .then_ignore(end())
    .boxed()
}

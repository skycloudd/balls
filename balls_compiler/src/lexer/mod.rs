use balls_span::Span;
use chumsky::{input::WithContext, prelude::*};
use token::{Kw, Punc, Simple, Token, Tokens};

pub mod token;

pub fn lexer<'src>() -> impl Parser<
    'src,
    WithContext<Span, &'src str>,
    Tokens<'src>,
    extra::Err<Rich<'src, char, Span, &'src str>>,
> {
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

        let keyword = choice((
            text::keyword("let").to(Simple::Kw(Kw::Let)),
            text::keyword("match").to(Simple::Kw(Kw::Match)),
            text::keyword("with").to(Simple::Kw(Kw::With)),
        ))
        .boxed();

        let punctuation = choice((
            // 2 char
            //
            just("==").to(Simple::Punc(Punc::DoubleEquals)),
            just("!=").to(Simple::Punc(Punc::NotEquals)),
            just("->").to(Simple::Punc(Punc::SingleArrow)),
            // 1 char
            //
            just("=").to(Simple::Punc(Punc::Equals)),
            just('.').to(Simple::Punc(Punc::Period)),
            just(',').to(Simple::Punc(Punc::Comma)),
            just('!').to(Simple::Punc(Punc::Exclamation)),
            just('|').to(Simple::Punc(Punc::Pipe)),
            just(":").to(Simple::Punc(Punc::Colon)),
            // arithmetic op
            just('+').to(Simple::Punc(Punc::Plus)),
            just('-').to(Simple::Punc(Punc::Minus)),
            just('*').to(Simple::Punc(Punc::Star)),
            just('/').to(Simple::Punc(Punc::Slash)),
            // comparison op
            just("<=").to(Simple::Punc(Punc::LessThanEqual)),
            just(">=").to(Simple::Punc(Punc::GreaterThanEqual)),
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
            .clone()
            .delimited_by(just('('), just(')'))
            .recover_with(via_parser(nested_delimiters(
                '(',
                ')',
                [('{', '}')],
                |span| Tokens(vec![(Token::Error, span)]),
            )))
            .map(Token::Parentheses)
            .boxed();

        let curly_braces = tokens
            .delimited_by(just('{'), just('}'))
            .recover_with(via_parser(nested_delimiters(
                '{',
                '}',
                [('(', ')')],
                |span| Tokens(vec![(Token::Error, span)]),
            )))
            .map(Token::CurlyBraces)
            .boxed();

        let token = choice((simple, parenthesised, curly_braces))
            .map_with(|tok, err| (tok, err.span()))
            .padded_by(comment.clone().repeated())
            .padded()
            .boxed();

        token.repeated().collect().padded().map(Tokens).boxed()
    })
    .then_ignore(end())
    .boxed()
}

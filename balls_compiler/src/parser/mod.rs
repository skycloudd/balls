use crate::lexer::token::{self, Kw, Punc, Token};
use ast::{Arg, Ast, BinaryOp, Expr, Function, PostfixOp, UnaryOp};
use balls_span::{Span, Spanned};
use chumsky::{input::SpannedInput, prelude::*};

pub mod ast;

pub fn parser<'tok>() -> impl Parser<
    'tok,
    SpannedInput<Token, Span, &'tok [(Token, Span)]>,
    Spanned<Ast>,
    extra::Err<Rich<'tok, Token, Span, &'static str>>,
> {
    function_parser()
        .repeated()
        .collect()
        .map_with(|functions, e| Spanned(Ast { functions }, e.span()))
        .boxed()
}

fn function_parser<'tok>() -> impl Parser<
    'tok,
    SpannedInput<Token, Span, &'tok [(Token, Span)]>,
    Spanned<ast::Function>,
    extra::Err<Rich<'tok, Token, Span, &'static str>>,
> {
    let name = ident_parser();

    let parameter = ident_parser()
        .then_ignore(just(Token::Simple(token::Simple::Punc(Punc::DoubleColon))))
        .then(ty_parser())
        .map_with(|(name, ty), e| Spanned(Arg { name, ty }, e.span()))
        .boxed();

    let parameters = parameter
        .separated_by(just(Token::Simple(token::Simple::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .nested_in(select_ref! {
            Token::Parentheses(tokens) = e => tokens.0.as_slice().spanned(Span::to_end(&e.span()))
        })
        .map_with(|parameters, e| Spanned(parameters, e.span()))
        .boxed();

    just(Token::Simple(token::Simple::Kw(Kw::Let)))
        .ignore_then(name)
        .then(parameters)
        .then_ignore(just(Token::Simple(token::Simple::Punc(Punc::DoubleColon))))
        .then(ty_parser())
        .then_ignore(just(Token::Simple(token::Simple::Punc(Punc::Equals))))
        .then(expr_parser())
        .map_with(|(((name, parameters), return_ty), body), e| {
            Spanned(
                Function {
                    name,
                    parameters,
                    return_ty,
                    body,
                },
                e.span(),
            )
        })
        .boxed()
}

#[allow(clippy::too_many_lines)]
fn expr_parser<'tok>() -> impl Parser<
    'tok,
    SpannedInput<Token, Span, &'tok [(Token, Span)]>,
    Spanned<Expr>,
    extra::Err<Rich<'tok, Token, Span, &'static str>>,
> {
    recursive(|expr| {
        let variable = ident_parser()
            .map_with(|ident, e| Spanned(Expr::Ident(ident), e.span()))
            .boxed();

        let boolean = select! {
            Token::Simple(token::Simple::Boolean(b)) => Expr::Boolean(b)
        }
        .map_with(|b, e| Spanned(b, e.span()))
        .boxed();

        let integer = select! {
            Token::Simple(token::Simple::Integer(i)) => Expr::Integer(i)
        }
        .map_with(|i, e| Spanned(i, e.span()))
        .boxed();

        let float = select! {
            Token::Simple(token::Simple::Float(f)) => Expr::Float(f)
        }
        .map_with(|f, e| Spanned(f, e.span()))
        .boxed();

        let parenthesized_expr = expr
            .clone()
            .nested_in(select_ref! {
                Token::Parentheses(tokens) = e => tokens.0.as_slice().spanned(Span::to_end(&e.span()))
            })
            .boxed();

        let lazy_expr = expr
            .clone()
            .nested_in(select_ref! {
                Token::CurlyBraces(tokens) = e => tokens.0.as_slice().spanned(Span::to_end(&e.span()))
            })
            .map_with(|expr: Spanned<Expr>, e| Spanned(Expr::Lazy(expr.boxed()), e.span()))
            .boxed();

        let atom = choice((
            variable,
            boolean,
            integer,
            float,
            parenthesized_expr,
            lazy_expr,
        ))
        .boxed();

        let call_args = expr
            .separated_by(just(Token::Simple(token::Simple::Punc( Punc::Comma))))
            .allow_trailing()
            .collect()
            .nested_in(select_ref! {
                Token::Parentheses(tokens) = e => tokens.0.as_slice().spanned(Span::to_end(&e.span()))
            })
            .map_with(|args, e|  Spanned(args, e.span()))
            .map(PostfixOp::Call)
            .boxed();

        let field_access = just(Token::Simple(token::Simple::Punc(Punc::Period)))
            .ignore_then(ident_parser())
            .map(PostfixOp::FieldAccess)
            .boxed();

        let postfix_op = choice((call_args, field_access)).map_with(|op, e| Spanned(op, e.span()));

        let postfix = atom
            .foldl(postfix_op.repeated(), |a, b| {
                let span = a.1.union(b.1);

                Spanned(
                    Expr::Postfix {
                        expr: a.boxed(),
                        op: b,
                    },
                    span,
                )
            })
            .boxed();

        let unary_op = choice((
            just(Token::Simple(token::Simple::Punc(Punc::Minus))).to(UnaryOp::Negate),
            (just(Token::Simple(token::Simple::Punc(Punc::Exclamation))).to(UnaryOp::Not)),
        ))
        .map_with(|op, e| Spanned(op, e.span()));

        let unary = unary_op
            .repeated()
            .foldr(postfix, |op, expr| {
                let span = op.1.union(expr.1);

                Spanned(
                    Expr::Unary {
                        op,
                        expr: expr.boxed(),
                    },
                    span,
                )
            })
            .boxed();

        let factor_op = choice((
            just(Token::Simple(token::Simple::Punc(Punc::Star))).to(BinaryOp::Multiply),
            just(Token::Simple(token::Simple::Punc(Punc::Slash))).to(BinaryOp::Divide),
        ))
        .map_with(|op, e| Spanned(op, e.span()));

        let factor = unary
            .clone()
            .foldl(factor_op.then(unary).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.union(rhs.1);

                Spanned(
                    Expr::Binary {
                        op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                    span,
                )
            })
            .boxed();

        let sum_op = choice((
            just(Token::Simple(token::Simple::Punc(Punc::Plus))).to(BinaryOp::Add),
            just(Token::Simple(token::Simple::Punc(Punc::Minus))).to(BinaryOp::Subtract),
        ))
        .map_with(|op, e| Spanned(op, e.span()));

        let sum = factor
            .clone()
            .foldl(sum_op.then(factor).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.union(rhs.1);

                Spanned(
                    Expr::Binary {
                        op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                    span,
                )
            })
            .boxed();

        let relational_op = choice((
            just(Token::Simple(token::Simple::Punc(Punc::LessThan))).to(BinaryOp::LessThan),
            just(Token::Simple(token::Simple::Punc(Punc::GreaterThan))).to(BinaryOp::GreaterThan),
            just(Token::Simple(token::Simple::Punc(Punc::LessThanEqual)))
                .to(BinaryOp::LessThanEqual),
            just(Token::Simple(token::Simple::Punc(Punc::GreaterThanEqual)))
                .to(BinaryOp::GreaterThanEqual),
        ))
        .map_with(|op, e| Spanned(op, e.span()));

        let relational = sum
            .clone()
            .foldl(relational_op.then(sum).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.union(rhs.1);

                Spanned(
                    Expr::Binary {
                        op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                    span,
                )
            })
            .boxed();

        let equality_op = choice((
            just(Token::Simple(token::Simple::Punc(Punc::DoubleEquals))).to(BinaryOp::Equals),
            just(Token::Simple(token::Simple::Punc(Punc::NotEquals))).to(BinaryOp::NotEquals),
        ))
        .map_with(|op, e| Spanned(op, e.span()));

        let equality = relational
            .clone()
            .foldl(equality_op.then(relational).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.union(rhs.1);

                Spanned(
                    Expr::Binary {
                        op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                    span,
                )
            })
            .boxed();

        equality
    })
}

fn ident_parser<'tok>() -> impl Parser<
    'tok,
    SpannedInput<Token, Span, &'tok [(Token, Span)]>,
    Spanned<ast::Ident>,
    extra::Err<Rich<'tok, Token, Span, &'static str>>,
> {
    select! {
        Token::Simple(token::Simple::Ident(ident)) => ast::Ident(ident)
    }
    .map_with(|ident, e| Spanned(ident, e.span()))
}

fn ty_parser<'tok>() -> impl Parser<
    'tok,
    SpannedInput<Token, Span, &'tok [(Token, Span)]>,
    Spanned<ast::Type>,
    extra::Err<Rich<'tok, Token, Span, &'static str>>,
> {
    select! {
        Token::Simple(token::Simple::Ident(ident)) => ast::Type(ident)
    }
    .map_with(|ty, e| Spanned(ty, e.span()))
    .boxed()
}

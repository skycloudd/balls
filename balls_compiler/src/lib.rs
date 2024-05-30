use balls_bytecode::Bytecode;
use balls_span::{Ctx, Span};
use camino::Utf8Path;
use chumsky::prelude::*;
use clap::{builder::PossibleValue, ValueEnum};
use codespan_reporting::files::SimpleFiles;
use diagnostics::Diagnostics;
use ptree::print_tree;

pub mod diagnostics;
mod lexer;
mod parser;

#[derive(Debug)]
pub struct Compiler<'a, 'file> {
    files: &'a mut SimpleFiles<&'file Utf8Path, &'static str>,
    print: Option<Print>,
}

impl<'a, 'file> Compiler<'a, 'file> {
    #[must_use]
    pub fn new(
        files: &'a mut SimpleFiles<&'file Utf8Path, &'static str>,
        print: Option<Print>,
    ) -> Self {
        Self { files, print }
    }

    /// Compiles the given source code into bytecode.
    ///
    /// # Errors
    ///
    /// Returns an error on I/O errors.
    pub fn compile(
        &mut self,
        source_code: &str,
        filename: &'file Utf8Path,
    ) -> std::io::Result<(Option<Bytecode>, Diagnostics)> {
        let source_code: &'static str = Box::leak(source_code.into());

        let file_id = self.files.add(filename, source_code);

        let file_ctx = Ctx(file_id);

        let mut diagnostics = Diagnostics::new();

        let (tokens, lex_errors) = lexer::lexer()
            .parse(source_code.with_context(file_ctx))
            .into_output_errors();

        diagnostics.add_errors(
            lex_errors
                .into_iter()
                .map(|err| err.map_token(|token| token.to_string()))
                .flat_map(|err| diagnostics::error::convert(&err)),
        );

        if self.print == Some(Print::Tokens) {
            if let Some(tokens) = tokens.as_ref() {
                print_tree(&tokens.clone())?;
            } else {
                println!("Unable to print tokens due to lexer errors");
            }
        }

        let tokens = tokens.map(|tokens| {
            let eof = tokens
                .0
                .last()
                .map_or_else(|| Span::new(file_ctx, 0..0), |(_, span)| span.to_end());

            let tokens = tokens.0.into_iter().collect::<Vec<_>>();

            (tokens, eof)
        });

        let (ast, parse_errors) = tokens
            .as_ref()
            .map_or_else(Default::default, |(tokens, eof)| {
                parser::parser()
                    .parse(tokens.spanned(*eof))
                    .into_output_errors()
            });

        diagnostics.add_errors(
            parse_errors
                .into_iter()
                .map(|err| err.map_token(|token| token.to_string()))
                .flat_map(|err| diagnostics::error::convert(&err)),
        );

        if self.print == Some(Print::Ast) {
            if let Some(ast) = ast {
                println!("{ast:?}");
            } else {
                println!("Unable to print AST due to parser errors");
            }
        }

        if !diagnostics.errors().is_empty() {
            return Ok((None, diagnostics));
        }

        std::process::exit(1);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Print {
    Tokens,
    Ast,
}

impl ValueEnum for Print {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Tokens, Self::Ast]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        match self {
            Self::Tokens => Some(PossibleValue::new("tokens")),
            Self::Ast => Some(PossibleValue::new("ast")),
        }
    }
}

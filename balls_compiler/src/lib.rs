use balls_bytecode::Bytecode;
use balls_span::{Ctx, Span};
use camino::Utf8Path;
use chumsky::prelude::*;
use codespan_reporting::files::SimpleFiles;
use diagnostics::Diagnostics;
use lasso::ThreadedRodeo;
use once_cell::sync::Lazy;
use typecheck::Typechecker;

pub mod diagnostics;
mod lexer;
mod parser;
mod scopes;
mod typecheck;

static RODEO: Lazy<ThreadedRodeo> = Lazy::new(ThreadedRodeo::new);

#[derive(Debug)]
pub struct Compiler<'a, 'file, 'src> {
    files: &'a mut SimpleFiles<&'file Utf8Path, &'src str>,
}

impl<'a, 'file, 'src> Compiler<'a, 'file, 'src> {
    #[must_use]
    pub fn new(files: &'a mut SimpleFiles<&'file Utf8Path, &'src str>) -> Self {
        Self { files }
    }

    /// Compiles the given source code into bytecode.
    ///
    /// # Errors
    ///
    /// Returns an error on I/O errors.
    pub fn compile(
        &mut self,
        source_code: &'src str,
        filename: &'file Utf8Path,
        debug: bool,
    ) -> std::io::Result<(Option<Bytecode>, Diagnostics)> {
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

        let typed_ast = ast.map(|ast| Typechecker::new(&mut diagnostics).typecheck(ast));

        if debug {
            println!("{typed_ast:?}");
        }

        if !diagnostics.errors().is_empty() {
            return Ok((None, diagnostics));
        }

        std::process::exit(1);
    }
}

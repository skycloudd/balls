use camino::Utf8Path;
use chumsky::prelude::*;
use codespan_reporting::files::SimpleFiles;
use diagnostics::Diagnostics;
use lasso::ThreadedRodeo;
use lowering::{mir::Mir, Lower};
use once_cell::sync::Lazy;
use span::{Ctx, Span};
use typecheck::Typechecker;

pub mod diagnostics;
mod lexer;
mod lowering;
mod parser;
mod scopes;
mod span;
mod typecheck;

static RODEO: Lazy<ThreadedRodeo> = Lazy::new(ThreadedRodeo::new);

#[derive(Debug)]
pub struct Compiler<'a, 'file, 'src> {
    files: &'a mut SimpleFiles<&'file Utf8Path, &'src str>,
}

impl<'a, 'file, 'src> Compiler<'a, 'file, 'src> {
    /// Creates a new [`Compiler`].
    #[must_use]
    pub fn new(files: &'a mut SimpleFiles<&'file Utf8Path, &'src str>) -> Self {
        Self { files }
    }

    /// Compiles the given source code into a [`TypedAst`].
    ///
    /// # Errors
    ///
    /// Returns an error on I/O errors.
    #[allow(clippy::missing_panics_doc)]
    pub fn compile(
        &mut self,
        source_code: &'src str,
        filename: &'file Utf8Path,
    ) -> std::io::Result<(Option<Mir>, Diagnostics)> {
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

        if !diagnostics.errors().is_empty() {
            return Ok((None, diagnostics));
        }

        #[allow(clippy::unwrap_used)]
        let typed_ast = typed_ast.unwrap();
        let mir = Lower::new().lower(typed_ast);

        Ok((Some(mir), diagnostics))
    }
}

fn join_comma(iter: impl IntoIterator<Item = impl core::fmt::Display>) -> String {
    iter.into_iter()
        .map(|item| item.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

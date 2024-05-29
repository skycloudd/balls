use balls_bytecode::Bytecode;
use balls_span::Ctx;
use camino::Utf8Path;
use chumsky::prelude::*;
use codespan_reporting::files::SimpleFiles;
use diagnostics::Diagnostics;

mod diagnostics;
mod lexer;
pub mod span;

#[derive(Debug)]
pub struct Compiler<'a> {
    files: SimpleFiles<&'a Utf8Path, &'static str>,
}

impl<'a> Compiler<'a> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
        }
    }

    /// Compiles the given source code into bytecode.
    ///
    /// # Errors
    ///
    /// Returns a list of [`Error`]s if the program fails to compile.
    pub fn compile(
        &mut self,
        source_code: &str,
        filename: &'a Utf8Path,
    ) -> (Option<Bytecode>, Diagnostics) {
        self.run(source_code, filename)
    }

    fn run(
        &mut self,
        source_code: &str,
        filename: &'a Utf8Path,
    ) -> (Option<Bytecode>, Diagnostics) {
        let source_code: &'static str = Box::leak(source_code.into());

        let file_id = self.files.add(filename, source_code);

        let file_ctx = Ctx(file_id);

        // let mut errors = Vec::new();
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

        if !diagnostics.errors().is_empty() {
            return (None, diagnostics);
        }

        todo!("{:#?}", tokens.unwrap());
    }
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self::new()
    }
}

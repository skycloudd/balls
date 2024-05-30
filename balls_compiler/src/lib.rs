use balls_bytecode::Bytecode;
use balls_span::Ctx;
use camino::Utf8Path;
use chumsky::prelude::*;
use clap::{builder::PossibleValue, ValueEnum};
use codespan_reporting::files::SimpleFiles;
use diagnostics::Diagnostics;
use ptree::print_tree;

pub mod diagnostics;
mod lexer;

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
    /// Returns a list of [`Error`]s if the program fails to compile.
    pub fn compile(
        &mut self,
        source_code: &str,
        filename: &'file Utf8Path,
    ) -> (Option<Bytecode>, Diagnostics) {
        self.run(source_code, filename)
    }

    fn run(
        &mut self,
        source_code: &str,
        filename: &'file Utf8Path,
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

        if self.print == Some(Print::Tokens) {
            print_tree(&tokens.unwrap()).unwrap();
        }

        todo!()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Print {
    Tokens,
}

impl ValueEnum for Print {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Tokens]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        match self {
            Self::Tokens => Some(PossibleValue::new("tokens")),
        }
    }
}

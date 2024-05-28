use balls_bytecode::Bytecode;
use balls_span::Ctx;
use camino::Utf8Path;
use chumsky::prelude::*;
use codespan_reporting::files::SimpleFiles;

mod lexer;
pub mod span;

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

    pub fn compile(&mut self, source_code: &str, filename: &'a Utf8Path) -> Bytecode {
        self.run(source_code, filename)
    }

    fn run(&mut self, source_code: &str, filename: &'a Utf8Path) -> Bytecode {
        let source_code: &'static str = Box::leak(source_code.into());

        let file_id = self.files.add(filename, source_code);

        let file_ctx = Ctx(file_id);

        let tokens = lexer::lexer()
            .parse(source_code.with_context(file_ctx))
            .into_output_errors();

        todo!("{tokens:?}");
    }
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self::new()
    }
}

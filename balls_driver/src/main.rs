use balls_compiler::{diagnostics::report, Compiler};
use camino::Utf8PathBuf;
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

#[derive(Parser)]
struct Args {
    path: Utf8PathBuf,

    #[clap(short, long)]
    debug: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let source_code = std::fs::read_to_string(&args.path)?;

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let mut files = SimpleFiles::new();

    let typed_ast = match Compiler::new(&mut files).compile(&source_code, &args.path)? {
        (Some(typed_ast), diagnostics) => {
            assert!(diagnostics.errors().is_empty());

            for warning in diagnostics.warnings() {
                let diag = report(warning);

                term::emit(&mut writer.lock(), &config, &files, &diag)?;
            }

            typed_ast
        }
        (None, diagnostics) => {
            assert!(!diagnostics.errors().is_empty());

            for error in diagnostics.errors() {
                let diag = report(error);

                term::emit(&mut writer.lock(), &config, &files, &diag)?;
            }

            std::process::exit(1);
        }
    };

    if args.debug {
        println!("{typed_ast:#?}");
    }

    Ok(())
}

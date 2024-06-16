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
use tracing_subscriber::{layer::SubscriberExt, Registry};
use tracing_tree::HierarchicalLayer;

#[derive(Parser)]
struct Args {
    path: Utf8PathBuf,

    #[clap(short, long)]
    debug: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let subscriber = Registry::default().with(
        HierarchicalLayer::default()
            // .with_indent_lines(true)
            .with_targets(true)
            .with_deferred_spans(true)
            .with_higher_precision(true),
    );

    tracing::subscriber::set_global_default(subscriber).unwrap();

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

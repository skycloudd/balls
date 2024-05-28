use balls_compiler::Compiler;
use balls_vm::Vm;
use camino::Utf8PathBuf;
use clap::Parser;

#[derive(Parser)]
struct Args {
    path: Utf8PathBuf,
}

fn main() {
    let args = Args::parse();

    let source_code = std::fs::read_to_string(&args.path).unwrap();

    let bc = Compiler::new().compile(&source_code, &args.path);

    Vm::<2>::new(bc).run();
}

use compiler::Compiler;
use vm::Vm;

fn main() {
    let bc = Compiler::compile();

    let mut vm = Vm::<2>::new(bc);

    vm.run();
}

use bytecode::{Bytecode, ConstId, Constants, Instruction, RegId, Value};
use vm::Vm;

fn main() {
    let bc = Bytecode::new(
        vec![
            Instruction::LoadConstant {
                constant: ConstId::new(0),
                reg: RegId::new(0),
            },
            Instruction::LoadConstant {
                constant: ConstId::new(1),
                reg: RegId::new(1),
            },
            Instruction::Add {
                out: RegId::new(0),
                a: RegId::new(0),
                b: RegId::new(1),
            },
            Instruction::Print { reg: RegId::new(0) },
        ],
        Constants::new(vec![Value::Int(34), Value::Int(35)]),
    );

    let mut vm = Vm::<2>::new(bc);

    vm.run();
}

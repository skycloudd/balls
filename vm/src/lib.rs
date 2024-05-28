use std::ops::{Index, IndexMut};

use bytecode::{Bytecode, Instruction, RegId, Value};

pub struct Vm<const REGS: usize> {
    bytecode: Bytecode,
    registers: Registers<REGS>,

    ip: usize,
}

impl<const REGS: usize> Vm<REGS> {
    pub fn new(bytecode: Bytecode) -> Self {
        const NONE: Option<Value> = None;

        let registers = Registers::new([NONE; REGS]);

        Self {
            bytecode,
            registers,
            ip: 0,
        }
    }

    pub fn run(&mut self) {
        while self.ip < self.bytecode.code().len() {
            self.execute();
        }
    }

    fn execute(&mut self) {
        let instruction = &self.bytecode.code()[self.ip];

        match instruction {
            Instruction::LoadConstant { constant, reg } => {
                self.registers[*reg] = Some(self.bytecode.constants()[*constant].clone());
            }
            Instruction::Add { out, a, b } => {
                let a = self.registers[*a].as_ref().unwrap();
                let b = self.registers[*b].as_ref().unwrap();

                self.registers[*out] = Some(a.add(b));
            }
            Instruction::Sub { out, a, b } => {
                let a = self.registers[*a].as_ref().unwrap();
                let b = self.registers[*b].as_ref().unwrap();

                self.registers[*out] = Some(a.sub(b));
            }
            Instruction::Mul { out, a, b } => {
                let a = self.registers[*a].as_ref().unwrap();
                let b = self.registers[*b].as_ref().unwrap();

                self.registers[*out] = Some(a.mul(b));
            }
            Instruction::Div { out, a, b } => {
                let a = self.registers[*a].as_ref().unwrap();
                let b = self.registers[*b].as_ref().unwrap();

                self.registers[*out] = Some(a.div(b));
            }
            Instruction::Print { reg } => {
                let value = self.registers[*reg].as_ref().unwrap();

                println!("{}", value);
            }
        }

        self.ip += 1;
    }
}

struct Registers<const REGS: usize>([Option<Value>; REGS]);

impl<const REGS: usize> Registers<REGS> {
    fn new(registers: [Option<Value>; REGS]) -> Self {
        Self(registers)
    }
}

impl<const REGS: usize> Index<RegId> for Registers<REGS> {
    type Output = Option<Value>;

    fn index(&self, index: RegId) -> &Self::Output {
        &self.0[index.id()]
    }
}

impl<const REGS: usize> IndexMut<RegId> for Registers<REGS> {
    fn index_mut(&mut self, index: RegId) -> &mut Self::Output {
        &mut self.0[index.id()]
    }
}

#[cfg(test)]
mod tests {}

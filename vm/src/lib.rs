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
            match self.execute() {
                Ok(()) => {}
                Err(e) => {
                    eprintln!("error: {}", e);
                    break;
                }
            }
        }
    }

    fn execute(&mut self) -> Result<(), &'static str> {
        let instruction = &self.bytecode.code()[self.ip];

        match instruction {
            Instruction::LoadConstant { constant, reg } => {
                self.registers[*reg] = Some(self.bytecode.constants()[*constant].clone());
            }
            Instruction::Add { out, a, b } => {
                let a = self.reg(*a);
                let b = self.reg(*b);

                self.registers[*out] = Some(a.add(b));
            }
            Instruction::Sub { out, a, b } => {
                let a = self.reg(*a);
                let b = self.reg(*b);

                self.registers[*out] = Some(a.sub(b));
            }
            Instruction::Mul { out, a, b } => {
                let a = self.reg(*a);
                let b = self.reg(*b);

                self.registers[*out] = Some(a.mul(b));
            }
            Instruction::Div { out, a, b } => {
                let a = self.reg(*a);
                let b = self.reg(*b);

                self.registers[*out] = Some(a.div(b)?);
            }
            Instruction::Print { reg } => {
                let value = self.reg(*reg);

                println!("{}", value);
            }
        }

        self.ip += 1;

        Ok(())
    }

    fn reg(&self, reg: RegId) -> &Value {
        self.registers[reg].as_ref().unwrap()
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
        if index.id() >= REGS {
            panic!(
                "register out of bounds: {}, registers: {}",
                index.id(),
                REGS
            );
        }

        &self.0[index.id()]
    }
}

impl<const REGS: usize> IndexMut<RegId> for Registers<REGS> {
    fn index_mut(&mut self, index: RegId) -> &mut Self::Output {
        if index.id() >= REGS {
            panic!(
                "register out of bounds: {}, registers: {}",
                index.id(),
                REGS
            );
        }

        &mut self.0[index.id()]
    }
}

trait Arithmetic {
    fn add(&self, other: &Self) -> Self;
    fn sub(&self, other: &Self) -> Self;
    fn mul(&self, other: &Self) -> Self;
    fn div(&self, other: &Self) -> Result<Self, &'static str>
    where
        Self: Sized;
}

impl Arithmetic for Value {
    fn add(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a + b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a + b),

            _ => panic!("cannot add {:?} and {:?}", self, other),
        }
    }

    fn sub(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a - b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a - b),

            _ => panic!("cannot sub {:?} and {:?}", self, other),
        }
    }

    fn mul(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a * b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a * b),

            _ => panic!("cannot mul {:?} and {:?}", self, other),
        }
    }

    fn div(&self, other: &Self) -> Result<Self, &'static str> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => {
                if *b != 0 {
                    Ok(Self::Int(a / b))
                } else {
                    Err("cannot divide by zero")
                }
            }
            (Self::Float(a), Self::Float(b)) => {
                if *b != 0.0 {
                    Ok(Self::Float(a / b))
                } else {
                    Err("cannot divide by zero")
                }
            }

            _ => panic!("cannot div {:?} and {:?}", self, other),
        }
    }
}

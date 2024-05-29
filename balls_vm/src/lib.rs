use balls_bytecode::{BinaryOp, Bytecode, Instruction, RegId, Value};
use core::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Vm<const REGS: usize> {
    bytecode: Bytecode,
    registers: Registers<REGS>,

    ip: usize,
}

impl<const REGS: usize> Vm<REGS> {
    #[must_use]
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
                Err(err) => {
                    eprintln!("error: {err}");
                    break;
                }
            }
        }
    }

    fn execute(&mut self) -> Result<(), &'static str> {
        let instruction = &self.bytecode.code()[self.ip];

        match &instruction.0 {
            Instruction::LoadConstant { constant, reg } => {
                self.registers[*reg] = Some(self.bytecode.constants()[*constant]);
            }
            Instruction::Binary { op, out, lhs, rhs } => {
                let lhs = self.reg(*lhs);
                let rhs = self.reg(*rhs);

                let result = match op {
                    BinaryOp::Add => lhs.add(rhs),
                    BinaryOp::Sub => lhs.sub(rhs),
                    BinaryOp::Mul => lhs.mul(rhs),
                    BinaryOp::Div => lhs.div(rhs)?,
                };

                self.registers[*out] = Some(result);
            }
            Instruction::Print { reg } => {
                let value = self.reg(*reg);

                println!("{value}");
            }
        }

        self.ip += 1;

        Ok(())
    }

    fn reg(&self, reg: RegId) -> &Value {
        self.registers[reg].as_ref().unwrap()
    }
}

#[derive(Debug)]
struct Registers<const REGS: usize>([Option<Value>; REGS]);

impl<const REGS: usize> Registers<REGS> {
    const fn new(registers: [Option<Value>; REGS]) -> Self {
        Self(registers)
    }
}

impl<const REGS: usize> Index<RegId> for Registers<REGS> {
    type Output = Option<Value>;

    fn index(&self, index: RegId) -> &Self::Output {
        assert!(
            index.id() < REGS,
            "register out of bounds: {}, registers: {REGS}",
            index.id(),
        );

        &self.0[index.id()]
    }
}

impl<const REGS: usize> IndexMut<RegId> for Registers<REGS> {
    fn index_mut(&mut self, index: RegId) -> &mut Self::Output {
        assert!(
            index.id() < REGS,
            "register out of bounds: {}, registers: {REGS}",
            index.id(),
        );

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
            (Self::Int(lhs), Self::Int(rhs)) => Self::Int(lhs + rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs + rhs),

            _ => panic!("cannot add {self:?} and {other:?}"),
        }
    }

    fn sub(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => Self::Int(lhs - rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs - rhs),

            _ => panic!("cannot sub {self:?} and {other:?}"),
        }
    }

    fn mul(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => Self::Int(lhs * rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs * rhs),

            _ => panic!("cannot mul {self:?} and {other:?}"),
        }
    }

    fn div(&self, other: &Self) -> Result<Self, &'static str> {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => {
                if *rhs != 0 {
                    Ok(Self::Int(lhs / rhs))
                } else {
                    Err("cannot divide by zero")
                }
            }
            (Self::Float(lhs), Self::Float(rhs)) => {
                if *rhs == 0.0 {
                    Err("cannot divide by zero")
                } else {
                    Ok(Self::Float(lhs / rhs))
                }
            }

            _ => panic!("cannot div {self:?} and {other:?}"),
        }
    }
}

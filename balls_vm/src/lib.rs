use balls_bytecode::{BinaryOp, Bytecode, Function, Instruction, UnaryOp, Value};
use lasso::Spur;
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct Vm {
    bytecode: Bytecode,
    stack: Vec<Vec<Value>>,
    ip: Vec<(usize, usize)>,
    functions: FxHashMap<Spur, Value>,
    names: Vec<FxHashMap<Spur, Value>>,
}

impl Vm {
    /// Create a new VM with the given bytecode.
    ///
    /// # Panics
    ///
    /// Panics if the bytecode does not have a main function set.
    #[must_use]
    pub fn new(bytecode: Bytecode) -> Self {
        let main = bytecode.get_main().expect("no main function");

        let functions = bytecode
            .functions()
            .iter()
            .enumerate()
            .map(|(idx, function)| (function.name(), Value::Function(idx)))
            .collect();

        Self {
            bytecode,
            stack: vec![],
            ip: vec![(main, 0)],
            functions,
            names: vec![],
        }
    }

    /// Run the VM.
    ///
    /// # Errors
    ///
    /// Returns an error if the VM encounters an error while running.
    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        while self.ip().is_some() {
            self.execute()?;
        }

        Ok(())
    }

    fn execute(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let instruction = self.current_instruction()?;

        match *instruction {
            Instruction::LoadName(name) => {
                let value = self
                    .names
                    .last()
                    .and_then(|scope| scope.get(&name))
                    .copied()
                    .or_else(|| self.functions.get(&name).copied())
                    .ok_or("name not found")?;

                self.push(value);
            }
            Instruction::StoreName(name) => {
                let value = self.pop()?;

                self.names
                    .last_mut()
                    .ok_or("no scope to store name")
                    .map(|scope| {
                        scope.insert(name, value);
                    })?;
            }
            Instruction::LoadConstant(value) => {
                self.push(value);
            }
            Instruction::Binary(op) => {
                let lhs = self.pop()?;
                let rhs = self.pop()?;

                let result = match op {
                    BinaryOp::Add => lhs.add(&rhs),
                    BinaryOp::Sub => lhs.sub(&rhs),
                    BinaryOp::Mul => lhs.mul(&rhs),
                    BinaryOp::Div => lhs.div(&rhs)?,
                    BinaryOp::LessThan => lhs.lt(&rhs).into(),
                    BinaryOp::GreaterThan => lhs.gt(&rhs).into(),
                    BinaryOp::LessThanEqual => lhs.le(&rhs).into(),
                    BinaryOp::GreaterThanEqual => lhs.ge(&rhs).into(),
                    BinaryOp::Equals => lhs.eq(&rhs).into(),
                    BinaryOp::NotEquals => lhs.ne(&rhs).into(),
                };

                self.push(result);
            }
            Instruction::Unary(op) => {
                let value = self.pop()?;

                let result = match op {
                    UnaryOp::Neg => value.neg(),
                    UnaryOp::Not => value.not(),
                };

                self.push(result);
            }
            Instruction::Return => {
                let value = self.pop()?;

                self.names.pop();
                self.stack.pop();

                if let Some(frame) = self.stack.last_mut() {
                    frame.push(value);
                }

                self.ip.pop();
            }
            Instruction::Call => {
                let function = match self.pop()? {
                    Value::Function(function) => function,
                    other => return Err(format!("cannot call non-function: {other:?}").into()),
                };

                self.names.push(FxHashMap::default());

                self.ip.push((function, 0));

                return Ok(());
            }
            Instruction::Swap => {
                let a = self.pop()?;
                let b = self.pop()?;

                self.push(a);
                self.push(b);
            }
            Instruction::Dup => {
                let value = self.pop()?;

                self.push(value);
                self.push(value);
            }
            Instruction::PushStackFrame => {
                self.stack.push(vec![]);
            }
            Instruction::Print => {
                let value = self.pop()?;

                println!("{value:?}");
            }
        }

        if let Some(ip) = self.ip_mut() {
            ip.1 += 1;
        }

        Ok(())
    }

    fn ip(&self) -> Option<(usize, usize)> {
        self.ip.last().copied()
    }

    fn ip_mut(&mut self) -> Option<&mut (usize, usize)> {
        self.ip.last_mut()
    }

    fn push(&mut self, value: Value) {
        self.stack
            .last_mut()
            .expect("function stack underflow")
            .push(value);
    }

    fn pop(&mut self) -> Result<Value, &'static str> {
        self.stack
            .last_mut()
            .ok_or("function stack underflow")?
            .pop()
            .ok_or("stack underflow")
    }

    fn current_function(&self) -> Result<&Function, &'static str> {
        self.bytecode
            .get_function(self.ip().expect("no ip").0)
            .ok_or("function not found")
    }

    fn current_instruction(&self) -> Result<&Instruction, &'static str> {
        self.current_function()?
            .code()
            .get(self.ip().expect("no ip").1)
            .ok_or("instruction not found")
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

trait Comparison {
    fn lt(&self, other: &Self) -> bool;
    fn gt(&self, other: &Self) -> bool;
    fn le(&self, other: &Self) -> bool;
    fn ge(&self, other: &Self) -> bool;
}

trait Unary {
    fn neg(&self) -> Self;
    fn not(&self) -> Self;
}

impl Arithmetic for Value {
    fn add(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => Self::Int(lhs + rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(*lhs + *rhs),

            _ => panic!("cannot add {self:?} and {other:?}"),
        }
    }

    fn sub(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => Self::Int(lhs - rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(*lhs - *rhs),

            _ => panic!("cannot sub {self:?} and {other:?}"),
        }
    }

    fn mul(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => Self::Int(lhs * rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(*lhs * *rhs),

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
                    Ok(Self::Float(*lhs / *rhs))
                }
            }

            _ => panic!("cannot div {self:?} and {other:?}"),
        }
    }
}

impl Comparison for Value {
    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs < rhs,
            (Self::Float(lhs), Self::Float(rhs)) => lhs < rhs,

            _ => panic!("cannot lt {self:?} and {other:?}"),
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs > rhs,
            (Self::Float(lhs), Self::Float(rhs)) => lhs > rhs,

            _ => panic!("cannot gt {self:?} and {other:?}"),
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs <= rhs,
            (Self::Float(lhs), Self::Float(rhs)) => lhs <= rhs,

            _ => panic!("cannot le {self:?} and {other:?}"),
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs >= rhs,
            (Self::Float(lhs), Self::Float(rhs)) => lhs >= rhs,

            _ => panic!("cannot ge {self:?} and {other:?}"),
        }
    }
}

impl Unary for Value {
    fn neg(&self) -> Self {
        match self {
            Self::Int(value) => Self::Int(-value),
            Self::Float(value) => Self::Float(-value),

            _ => panic!("cannot negate {self:?}"),
        }
    }

    fn not(&self) -> Self {
        match self {
            Self::Bool(value) => Self::Bool(!value),

            _ => panic!("cannot not {self:?}"),
        }
    }
}

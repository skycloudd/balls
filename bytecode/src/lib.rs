#[derive(Clone, Debug)]
pub struct Bytecode {
    code: Vec<Instruction>,
    constants: Constants,
}

impl Bytecode {
    pub fn new(code: Vec<Instruction>, constants: Constants) -> Self {
        Self { code, constants }
    }

    pub fn code(&self) -> &[Instruction] {
        &self.code
    }

    pub fn constants(&self) -> &Constants {
        &self.constants
    }
}

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum Instruction {
    LoadConstant { constant: ConstId, reg: RegId },
    Add { out: RegId, a: RegId, b: RegId },
    Sub { out: RegId, a: RegId, b: RegId },
    Mul { out: RegId, a: RegId, b: RegId },
    Div { out: RegId, a: RegId, b: RegId },
    Print { reg: RegId },
}

#[derive(Clone, Debug)]
pub struct Constants(Vec<Value>);

impl Constants {
    pub fn new(values: Vec<Value>) -> Self {
        Self(values)
    }
}

impl core::ops::Index<ConstId> for Constants {
    type Output = Value;

    fn index(&self, index: ConstId) -> &Self::Output {
        &self.0[index.0]
    }
}

#[derive(Clone, Copy, Debug)]
pub struct RegId(usize);

impl RegId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn id(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ConstId(usize);

impl ConstId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
}

impl Value {
    pub fn add(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a + b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a + b),

            _ => panic!("cannot add {:?} and {:?}", self, other),
        }
    }

    pub fn sub(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a - b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a - b),

            _ => panic!("cannot sub {:?} and {:?}", self, other),
        }
    }

    pub fn mul(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a * b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a * b),

            _ => panic!("cannot mul {:?} and {:?}", self, other),
        }
    }

    pub fn div(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a / b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a / b),

            _ => panic!("cannot div {:?} and {:?}", self, other),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(fl) => write!(f, "{}", fl),
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[cfg(test)]
mod tests {}

use balls_span::Spanned;

#[derive(Debug)]
pub struct Bytecode {
    code: Vec<Spanned<Instruction>>,
    constants: Constants,
}

impl Bytecode {
    #[must_use]
    pub fn new(code: Vec<Spanned<Instruction>>, constants: Constants) -> Self {
        Self { code, constants }
    }

    #[must_use]
    pub fn code(&self) -> &[Spanned<Instruction>] {
        &self.code
    }

    #[must_use]
    pub const fn constants(&self) -> &Constants {
        &self.constants
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    LoadConstant {
        constant: ConstId,
        reg: RegId,
    },
    Binary {
        op: BinaryOp,
        out: RegId,
        lhs: RegId,
        rhs: RegId,
    },
    Print {
        reg: RegId,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct Constants(Vec<Value>);

impl Constants {
    #[must_use]
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
    #[must_use]
    pub const fn new(id: usize) -> Self {
        Self(id)
    }

    #[must_use]
    pub const fn id(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ConstId(usize);

impl ConstId {
    #[must_use]
    pub const fn new(id: usize) -> Self {
        Self(id)
    }
}

pub type IntTy = i32;
pub type FloatTy = f32;

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
}

impl core::fmt::Display for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
        }
    }
}

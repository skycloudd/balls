use lasso::Spur;
use ordered_float::OrderedFloat;

#[derive(Debug, Default)]
pub struct Bytecode {
    functions: Vec<Function>,
    main: Option<usize>,
}

impl Bytecode {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            functions: vec![],
            main: None,
        }
    }

    pub fn add_function(&mut self, function: Function) {
        self.functions.push(function);
    }

    #[must_use]
    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    #[must_use]
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    pub fn set_main(&mut self, index: usize) {
        self.main = Some(index);
    }

    #[must_use]
    pub const fn get_main(&self) -> Option<usize> {
        self.main
    }
}

#[derive(Debug, Default)]
pub struct Function {
    name: Spur,
    code: Vec<Instruction>,
    params: Vec<Spur>,
}

impl Function {
    #[must_use]
    pub const fn new(name: Spur, params: Vec<Spur>) -> Self {
        Self {
            name,
            code: vec![],
            params,
        }
    }

    pub fn instr(&mut self, instruction: Instruction) -> usize {
        self.code.push(instruction);

        self.code.len() - 1
    }

    #[must_use]
    pub fn instr_at(&self, index: usize) -> &Instruction {
        &self.code[index]
    }

    pub fn instr_at_mut(&mut self, index: usize) -> &mut Instruction {
        &mut self.code[index]
    }

    #[must_use]
    pub const fn name(&self) -> Spur {
        self.name
    }

    #[must_use]
    pub fn code(&self) -> &[Instruction] {
        &self.code
    }

    #[must_use]
    pub fn params(&self) -> &[Spur] {
        &self.params
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    LoadName(Spur),
    StoreName(Spur),
    LoadConstant(Value),
    Binary(BinaryOp),
    Unary(UnaryOp),
    Return,
    Call,
    Swap,
    Dup,
    PushStackFrame,
    Print,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,

    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,

    Equals,
    NotEquals,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

pub type IntTy = i32;
pub type FloatTy = f32;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Value {
    Int(IntTy),
    Float(OrderedFloat<FloatTy>),
    Bool(bool),
    Function(usize),
}

impl core::fmt::Display for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::Function(v) => write!(f, "fn<{v}>"),
        }
    }
}

impl From<IntTy> for Value {
    fn from(value: IntTy) -> Self {
        Self::Int(value)
    }
}

impl From<FloatTy> for Value {
    fn from(value: FloatTy) -> Self {
        Self::Float(OrderedFloat(value))
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

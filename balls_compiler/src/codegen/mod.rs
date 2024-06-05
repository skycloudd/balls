use crate::{
    typecheck::typed_ast::{self, Expr, Function, TypedAst, TypedExpr},
    RODEO,
};
use balls_bytecode::{BinaryOp, Bytecode, Function as BytecodeFunction, Instruction, UnaryOp};

pub struct Codegen {
    bc: Bytecode,
    current_function: Option<BytecodeFunction>,
}

impl Codegen {
    pub const fn new() -> Self {
        Self {
            bc: Bytecode::new(),
            current_function: None,
        }
    }

    pub fn codegen(mut self, ast: TypedAst) -> Bytecode {
        for (idx, function) in ast.functions.into_iter().enumerate() {
            if RODEO.resolve(&function.0.name.0 .0) == "main" {
                self.bc.set_main(idx);
            }

            self.codegen_function(function.0);
        }

        self.bc
    }

    fn codegen_function(&mut self, function: Function) {
        self.current_function = Some(BytecodeFunction::new(
            function.name.0 .0,
            function
                .parameters
                .0
                .iter()
                .map(|param| param.0.name.0 .0)
                .collect(),
        ));

        for param in function.parameters.0.into_iter().rev() {
            self.instr(Instruction::StoreName(param.0.name.0 .0));
        }

        self.instr(Instruction::PushStackFrame);

        self.codegen_expr(function.body.0);

        self.instr(Instruction::Return);

        self.bc
            .add_function(self.current_function.take().expect("no current function"));
    }

    #[allow(clippy::too_many_lines)]
    fn codegen_expr(&mut self, expr: TypedExpr) {
        match expr.expr {
            Expr::Error => unreachable!(),
            Expr::Ident(name) => {
                self.instr(Instruction::LoadName(name.0 .0));
            }
            Expr::Integer(value) => {
                self.instr(Instruction::LoadConstant(value.into()));
            }
            Expr::Float(value) => {
                self.instr(Instruction::LoadConstant(value.into()));
            }
            Expr::Boolean(value) => {
                self.instr(Instruction::LoadConstant(value.into()));
            }
            Expr::Binary { op, lhs, rhs } => {
                self.codegen_expr(lhs.unbox().0);
                self.codegen_expr(rhs.unbox().0);

                let instr = Instruction::Binary(match op.0 {
                    typed_ast::BinaryOp::Add => BinaryOp::Add,
                    typed_ast::BinaryOp::Subtract => BinaryOp::Sub,
                    typed_ast::BinaryOp::Multiply => BinaryOp::Mul,
                    typed_ast::BinaryOp::Divide => BinaryOp::Div,
                    typed_ast::BinaryOp::LessThan => todo!(),
                    typed_ast::BinaryOp::GreaterThan => todo!(),
                    typed_ast::BinaryOp::LessThanEqual => todo!(),
                    typed_ast::BinaryOp::GreaterThanEqual => todo!(),
                    typed_ast::BinaryOp::Equals => todo!(),
                    typed_ast::BinaryOp::NotEquals => todo!(),
                });

                self.instr(instr);
            }
            Expr::Unary { op, expr: rhs } => {
                self.codegen_expr(rhs.unbox().0);

                let instr = Instruction::Unary(match op.0 {
                    typed_ast::UnaryOp::Negate => UnaryOp::Neg,
                    typed_ast::UnaryOp::Not => UnaryOp::Not,
                });

                self.instr(instr);
            }
            Expr::Postfix { expr: lhs, op } => {
                self.codegen_expr(lhs.unbox().0);

                match op.0 {
                    typed_ast::PostfixOp::Call(call_args) => {
                        for arg in call_args.0 {
                            self.codegen_expr(arg.0.clone());

                            self.instr(Instruction::Swap);
                        }

                        self.instr(Instruction::Call);
                    }
                    typed_ast::PostfixOp::FieldAccess(_field_name) => todo!(),
                }
            }
            Expr::Match {
                expr: _lhs,
                arms: _arms,
            } => todo!(),
            Expr::Print(lhs) => {
                self.codegen_expr(lhs.unbox().0);

                self.instr(Instruction::Dup);
                self.instr(Instruction::Print);
            }
        }
    }

    fn instr(&mut self, instr: Instruction) -> usize {
        self.current_function_mut().instr(instr)
    }

    fn current_function_mut(&mut self) -> &mut BytecodeFunction {
        self.current_function.as_mut().expect("no current function")
    }
}

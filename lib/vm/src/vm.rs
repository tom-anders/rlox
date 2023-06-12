use std::{ptr, marker::PhantomPinned, pin::Pin, ops::{DerefMut, Neg}};

use bytecode::{chunk::Chunk, instructions::Instruction, value::Value};
use compiler::Compiler;
use errors::RloxErrors;

#[derive(Debug, Clone)]
pub struct Vm {
    chunk: Chunk,
    stack: Vec<Value>,
    // FIXME: Using a raw pointer would be a bit more performant, but would also require `unsafe`.
    // So let's leave it like this for now and maybe optimize later.
    ip: usize,
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError(RloxErrors),
    RuntimeError(RuntimeError),
}

impl From<RloxErrors> for InterpretError {
    fn from(v: RloxErrors) -> Self {
        Self::CompileError(v)
    }
}

impl From<RuntimeError> for InterpretError {
    fn from(v: RuntimeError) -> Self {
        Self::RuntimeError(v)
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    InvalidNegateOperant(Value),
    InvalidBinaryOperants(Value, Value),
}

pub type Result<T> = std::result::Result<T, InterpretError>;

impl Vm {
    pub fn new(source: &str) -> Result<Self> {
        Ok(Self {
            chunk: Compiler::new(source).compile()?,
            ip: 0,
            stack: Vec::with_capacity(256),
        })
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            let bytes = &self.chunk.code()[self.ip..];
            let op = Instruction::from_bytes(bytes);

            log::trace!("Stack: {:?}", self.stack);
            log::trace!("{}", self.chunk.disassemble_instruction(self.ip).0);

            match op {
                Instruction::Return => {
                    println!("{}", self.pop());
                    return Ok(());
                }
                Instruction::Constant{ index } => {
                    let constant = self.chunk.constants().get(index as usize).unwrap();
                    self.push(constant.clone());
                }
                Instruction::Negate => {
                    let v = self.pop();
                    let neg = v.clone().neg().map_err(|v| RuntimeError::InvalidNegateOperant(v))?;
                    self.push(neg);
                }
                Instruction::Add => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push((a + b).map_err(|(a, b)| RuntimeError::InvalidBinaryOperants(a, b))?);
                }
                Instruction::Subtract => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push((a - b).map_err(|(a, b)| RuntimeError::InvalidBinaryOperants(a, b))?);
                }
                Instruction::Multiply => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push((a * b).map_err(|(a, b)| RuntimeError::InvalidBinaryOperants(a, b))?);
                }
                Instruction::Divide => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push((a / b).map_err(|(a, b)| RuntimeError::InvalidBinaryOperants(a, b))?);
                }
            }

            self.ip += op.num_bytes();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::println;

    use bytecode::{value::Value};
    use env_logger::Env;

    use super::*;

    #[test]
    fn smoke_test() {
        env_logger::init_from_env(Env::new().default_filter_or("trace"));
        println!();

        let mut vm = Vm::new("print 1 + 2").unwrap();
        vm.run().unwrap();
    }
}

use std::{ptr, marker::PhantomPinned, pin::Pin, ops::{DerefMut, Neg}, rc::Rc, collections::HashSet};

use bytecode::{chunk::Chunk, instructions::Instruction, value::Value};
use compiler::Compiler;
use errors::RloxErrors;

#[derive(Debug, Clone)]
pub struct Vm {
    chunk: Chunk,
    stack: Vec<Value>,
    strings: HashSet<Rc<String>>,
    // FIXME: Using a raw pointer would be a bit more performant, but would also require `unsafe`.
    // So let's leave it like this for now and maybe optimize later.
    ip: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum InterpretError {
    #[error(transparent)]
    CompileError(#[from] RloxErrors),
    #[error("line {line}: {error}")]
    RuntimeError{line: usize, error: String},
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum RuntimeError {
    #[error("Invalid negate operant: {0}")]
    InvalidNegateOperant(Value),
    #[error("Invalid binary operants: ({0}, {1})")]
    InvalidBinaryOperants(Value, Value),
}

pub type Result<T> = std::result::Result<T, InterpretError>;

impl bytecode::chunk::StringInterner for Vm {
    fn intern_string(&mut self, string: &mut Rc<String>) {
        match self.strings.get(string) {
            Some(s) => *string = s.clone(),
            None => {
                self.strings.insert(string.clone());
            }
        }
    }
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(256),
            ip: 0,
            strings: HashSet::new(),
            chunk: Chunk::default(),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }

    fn runtime_error(&self, error: RuntimeError) -> InterpretError {
        InterpretError::RuntimeError {
            line: self.chunk.lines()[self.ip],
            error: error.to_string(),
        }
    }

    pub fn run(&mut self, source: &str) -> Result<()> {
        let mut chunk = Compiler::new(source).compile()?;
        chunk.intern_strings(self);
        self.chunk = chunk;
        self.ip = 0;

        loop {
            let bytes = &self.chunk.code()[self.ip..];
            let op = Instruction::from_bytes(bytes);

            log::trace!("Stack: {:?}", self.stack);
            log::trace!("{}", self.chunk.disassemble_instruction(self.ip).0);

            match op {
                Instruction::Return => {
                    //  Exit interpreter
                    return Ok(());
                }
                Instruction::Print => {
                    println!("{}", self.pop());
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::Constant{ index } => {
                    let constant = self.chunk.constants().get(index as usize).unwrap();
                    self.push(constant.clone());
                }
                Instruction::Nil => self.push(Value::Nil),
                Instruction::True => self.push(Value::Boolean(true)),
                Instruction::False => self.push(Value::Boolean(false)),
                Instruction::Negate => {
                    let v = self.pop();
                    let neg = v.clone().neg().map_err(|v| self.runtime_error(RuntimeError::InvalidNegateOperant(v)))?;
                    self.push(neg);
                }
                Instruction::Not => {
                    let v = self.pop();
                    self.push(Value::Boolean(!v.is_truthy()));
                }
                Instruction::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Boolean(a == b));
                }
                Instruction::Less => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.less_than(b).map_err(|(a, b)| self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b)))?);
                }
                Instruction::Greater => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.greater_than(b).map_err(|(a, b)| self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b)))?);
                }
                Instruction::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    let mut result = (a + b).map_err(|(a, b)| self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b)))?;
                    result.intern_string(self);
                    self.push(result);
                }
                Instruction::Subtract => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a - b).map_err(|(a, b)| self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b)))?);
                }
                Instruction::Multiply => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a * b).map_err(|(a, b)| self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b)))?);
                }
                Instruction::Divide => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a / b).map_err(|(a, b)| self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b)))?);
                }
            }

            self.ip += op.num_bytes();
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
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

        Vm::new().run("print 1 + 2").unwrap();
    }
}

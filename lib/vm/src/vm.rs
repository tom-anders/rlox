use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    marker::PhantomPinned,
    ops::{DerefMut, Neg},
    pin::Pin,
    ptr,
    rc::Rc,
};

use bytecode::{
    chunk::Chunk,
    instructions::{Instruction, Jump},
    value::{Function, Object, ObjectData, RloxString, Value},
};
use compiler::Compiler;
use errors::RloxErrors;

#[derive(Debug, Clone)]
struct CallFrame {
    function: Function,
    // FIXME: Using a raw pointer would be a bit more performant, but would also require `unsafe`.
    // So let's leave it like this for now and maybe optimize later.
    ip: usize,
    base_slot: usize,
}

#[derive(Debug, Clone)]
pub struct Vm {
    stack: Vec<Value>,
    string_interner: Interner,
    globals: HashMap<Rc<String>, Value>,
    frames: Vec<CallFrame>,
}

#[derive(Debug, thiserror::Error)]
pub enum InterpretError {
    #[error(transparent)]
    CompileError(#[from] RloxErrors),
    #[error("line {line}: {error}")]
    RuntimeError { line: usize, error: String },
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum RuntimeError {
    #[error("Invalid negate operant: {0}")]
    InvalidNegateOperant(Value),
    #[error("Invalid binary operants: ({0}, {1})")]
    InvalidBinaryOperants(Value, Value),
    #[error("Undefined variable: '{0}'")]
    UndefinedVariable(String),
}

pub type Result<T> = std::result::Result<T, InterpretError>;

#[derive(Debug, Clone, Default)]
struct Interner {
    strings: HashSet<Rc<String>>,
}

impl bytecode::chunk::StringInterner for Interner {
    fn intern_string(&mut self, string: &mut RloxString) {
        match self.strings.get(&string.0) {
            Some(s) => string.0 = s.clone(),
            None => {
                self.strings.insert(string.0.clone());
            }
        }
    }
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(256),
            string_interner: Interner::default(),
            globals: HashMap::new(),
            frames: Vec::with_capacity(64),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }

    fn stack_at(&self, index: u8) -> &Value {
        let index = self.frame().base_slot + index as usize;
        self.stack.get(index).unwrap_or_else(|| panic!("Invalid stack index: {}", index))
    }

    fn stack_at_mut(&mut self, index: u8) -> &mut Value {
        let index = self.frame().base_slot + index as usize;
        self.stack.get_mut(index).unwrap_or_else(|| panic!("Invalid stack index: {}", index))
    }

    fn pop_n(&mut self, n: usize) {
        self.stack.truncate(self.stack.len() - n)
    }

    fn peek(&self) -> &Value {
        self.stack.last().expect("Stack underflow")
    }

    fn runtime_error(&self, error: RuntimeError) -> InterpretError {
        InterpretError::RuntimeError {
            line: self.frame().function.chunk.lines()[self.frame().ip],
            error: error.to_string(),
        }
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_chunk(&self) -> &Chunk {
        &self.frame().function.chunk
    }

    pub fn run_source(&mut self, source: &str) -> Result<()> {
        let mut function = Compiler::new(source).compile()?;
        function.intern_strings(&mut self.string_interner);

        let frame = CallFrame { function, ip: 0, base_slot: self.stack.len() };
        self.push_frame(frame);
        self.run()?;
        self.pop_frame();
        Ok(())
    }

    fn push_frame(&mut self, mut frame: CallFrame) {
        frame.function.intern_strings(&mut self.string_interner);
        self.push(Value::function(frame.function.clone()));
        self.frames.push(frame);
    }

    fn pop_frame(&mut self) -> CallFrame {
        self.frames.pop().unwrap()
    }

    fn run(&mut self) -> Result<()> {
        loop {
            let bytes = &self.frame_chunk().code()[self.frame().ip..];
            let op = Instruction::from_bytes(bytes);

            log::trace!("Stack: {:?}", self.stack);
            log::trace!("Globals: {:?}", self.globals);
            log::trace!("{}", self.frame_chunk().disassemble_instruction(self.frame().ip).0);

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
                Instruction::PopN(n) => {
                    self.pop_n(n as usize);
                }
                Instruction::Constant { index } => {
                    let constant = self.frame_chunk().constants().get(index as usize).unwrap();
                    self.push(constant.clone());
                }
                Instruction::DefineGlobal { constant_index } => {
                    let name = self
                        .frame_chunk()
                        .get_string_constant(constant_index)
                        .expect("Missing string constant for global");

                    self.globals.insert(name.0.clone(), self.peek().clone());
                    self.pop();
                }
                Instruction::SetGlobal { constant_index } => {
                    let name = self
                        .frame_chunk()
                        .get_string_constant(constant_index)
                        .expect("Missing string constant for global")
                        .clone();

                    let val = self.peek().clone();
                    match self.globals.entry(name.0.clone()) {
                        Entry::Occupied(mut entry) => {
                            *entry.get_mut() = val;
                        }
                        Entry::Vacant(_) => {
                            return Err(self
                                .runtime_error(RuntimeError::UndefinedVariable(name.to_string())))
                        }
                    };
                }
                Instruction::GetGlobal { constant_index } => {
                    let name = self
                        .frame_chunk()
                        .get_string_constant(constant_index)
                        .expect("Missing string constant for global");

                    // println!("Globals: {:?}", self.globals);
                    // println!("Globals: {:?}", self.globals.contains_key(name));

                    let value = self.globals.get(&name.0).ok_or(
                        self.runtime_error(RuntimeError::UndefinedVariable(name.to_string())),
                    )?;

                    self.push(value.clone());
                }
                Instruction::GetLocal { stack_slot } => {
                    self.push(self.stack_at(stack_slot).clone());
                }
                Instruction::SetLocal { stack_slot } => {
                    *self.stack_at_mut(stack_slot) = self.peek().clone();
                }
                Instruction::Nil => self.push(Value::Nil),
                Instruction::True => self.push(Value::Boolean(true)),
                Instruction::False => self.push(Value::Boolean(false)),
                Instruction::Negate => {
                    let v = self.pop();
                    let neg = v
                        .clone()
                        .neg()
                        .map_err(|v| self.runtime_error(RuntimeError::InvalidNegateOperant(v)))?;
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
                    self.push(a.less_than(b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?);
                }
                Instruction::Greater => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.greater_than(b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?);
                }
                Instruction::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    let mut result = (a + b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?;
                    result.intern_strings(&mut self.string_interner);
                    self.push(result);
                }
                Instruction::Subtract => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a - b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?);
                }
                Instruction::Multiply => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a * b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?);
                }
                Instruction::Divide => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a / b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?);
                }
                Instruction::JumpIfFalse(Jump(jump)) => {
                    if self.peek().is_falsey() {
                        self.frame_mut().ip += jump as usize;
                    }
                }
                Instruction::Jump(Jump(jump)) => {
                    self.frame_mut().ip += jump as usize;
                }
            }

            self.frame_mut().ip += op.num_bytes();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::println;

    use env_logger::Env;

    use super::*;

    #[test]
    fn smoke_test() {
        env_logger::init_from_env(Env::new().default_filter_or("trace"));
        println!();

        Vm::new().run_source("print 1 + 2;").unwrap();
    }
}

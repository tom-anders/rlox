use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, HashSet},
    marker::PhantomPinned,
    ops::{DerefMut, Neg},
    pin::Pin,
    println, ptr,
    rc::Rc, io::Write,
};

use bytecode::{
    chunk::{Chunk, StringInterner},
    instructions::{Instruction, Jump},
    value::{Function, NativeFun, Object, RloxString, Value},
};
use compiler::Compiler;
use errors::RloxErrors;
use itertools::Itertools;
use log::trace;

#[derive(Debug, Clone)]
struct CallFrame {
    function: Function,
    // FIXME: Using a raw pointer would be a bit more performant, but would also require `unsafe`.
    // So let's leave it like this for now and maybe optimize later.
    ip: usize,
    base_slot: usize,
}

impl CallFrame {
    pub fn new(function: Function, base_slot: usize) -> Self {
        Self { function, ip: 0, base_slot }
    }

    fn line(&self) -> usize {
        self.function.chunk.lines()[self.ip]
    }
}

#[derive(Debug, Clone)]
pub struct Vm {
    stack: Vec<Value>,
    string_interner: Interner,
    globals: HashMap<Rc<str>, Value>,
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
    #[error("{0} is not a function, can only call functions and classes")]
    NotAFunction(Value),
    #[error("Invalid argument count: expected {expected}, got {got}")]
    InvalidArgumentCount { expected: usize, got: usize },
}

pub type Result<T> = std::result::Result<T, InterpretError>;

#[derive(Debug, Clone, Default)]
struct Interner {
    strings: RefCell<HashSet<Rc<str>>>,
}

impl bytecode::chunk::StringInterner for Interner {
    fn intern_string(&self, string: &str) -> Rc<str> {
        let mut strings = self.strings.borrow_mut();
        match strings.get(string) {
            Some(s) => s.clone(),
            None => {
                let s = Rc::from(string);
                strings.insert(Rc::clone(&s));
                s
            }
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(256),
            string_interner: Interner::default(),
            globals: HashMap::new(),
            frames: Vec::with_capacity(64),
        };
        vm.define_native("clock", |_| {
            let now = std::time::SystemTime::now();
            let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
            Value::Number(duration.as_secs_f64())
        });
        vm
    }

    fn push(&mut self, value: Value) {
        trace!("Pushing {:?}", value);
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        trace!("Popping {:?}", self.stack.last());
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

    fn peek_n(&self, n: usize) -> &Value {
        self.stack.get(self.stack.len() - 1 - n).expect("Stack underflow")
    }

    pub fn stack_trace(&self) -> String {
        self.frames
            .iter()
            .rev()
            .map(|frame| {
                let name = if frame.function.name.0.is_empty() {
                    "script".to_string()
                } else {
                    format!("{}()", frame.function.name)
                };
                format!("[line {}] in {}", frame.line(), name)
            })
            .collect_vec()
            .join("\n")
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

    pub fn run_source(&mut self, source: &str, stdout: &mut impl Write) -> Result<()> {
        let mut function = Compiler::from_source(source, &self.string_interner).compile()?;

        self.push(function.clone().into());
        self.call(function.into(), 0).unwrap();
        self.run(stdout)?;
        Ok(())
    }

    fn push_frame(&mut self, mut frame: CallFrame) {
        trace!("Pushing frame: {:?}", frame);
        self.frames.push(frame);
    }

    fn pop_frame(&mut self) {
        let popped_frame = self.frames.pop().expect("callstack underflow");
        // +1 for the function itself
        self.pop_n(popped_frame.function.arity + 1);
    }

    fn call(&mut self, value: Value, arg_count: usize) -> Result<()> {
        match &value {
            Value::Object(ref o) => match o.as_ref() {
                Object::Function(function) => {
                    if function.arity != arg_count {
                        return Err(self.runtime_error(RuntimeError::InvalidArgumentCount {
                            expected: function.arity,
                            got: arg_count,
                        }));
                    }
                    trace!("stack: {}, len {}", self.stack.len(), arg_count);
                    self.push_frame(CallFrame::new(
                        function.clone(),
                        self.stack.len() - arg_count - 1,
                    ));
                    Ok(())
                }
                Object::NativeFun(native_fun) => {
                    let args = self.stack.split_off(self.stack.len() - arg_count);
                    let result = native_fun.0(args);
                    self.pop_n(arg_count);
                    self.push(result);
                    Ok(())
                }
                _ => Err(self.runtime_error(RuntimeError::NotAFunction(value))),
            },
            _ => Err(self.runtime_error(RuntimeError::NotAFunction(value))),
        }
    }

    fn define_native(&mut self, name: &str, native_fun: fn(Vec<Value>) -> Value) {
        self.push(Value::string(name, &self.string_interner));
        self.push(Value::native_function(NativeFun(native_fun)));

        self.globals.insert(self.peek_n(1).try_as_string().unwrap().clone().0, self.peek().clone());

        self.pop();
        self.pop();
    }

    fn run(&mut self, stdout: &mut impl Write) -> Result<()> {
        loop {
            let bytes = &self.frame_chunk().code()[self.frame().ip..];
            let op = Instruction::from_bytes(bytes);

            log::trace!("Stack: {:?}", self.stack);
            log::trace!("Globals: {:?}", self.globals);
            log::trace!("Constants: {:?}", self.frame_chunk().constants());
            log::trace!("{}", self.frame_chunk().disassemble_instruction(self.frame().ip).0);

            self.frame_mut().ip += op.num_bytes();

            match op {
                Instruction::Return => {
                    let result = self.pop();
                    self.pop_frame();
                    if self.frames.is_empty() {
                        return Ok(());
                    }
                    self.push(result);
                }
                Instruction::Print => {
                    writeln!(stdout, "{}", self.pop()).unwrap();
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::PopN(n) => {
                    self.pop_n(n as usize);
                }
                Instruction::Constant { index } => {
                    let constant = self.frame_chunk().constants().get(index as usize).unwrap();
                    trace!("Pushed constant: {:?}", constant);
                    self.push(constant.clone());
                }
                Instruction::DefineGlobal { constant_index } => {
                    let name = self
                        .frame_chunk()
                        .get_string_constant(constant_index)
                        .expect("Missing string constant for global");

                    trace!("Defining global: {:?} {:?}", name, self.peek());
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

                    let value = self.globals.get(&name.0).ok_or_else(
                        || self.runtime_error(RuntimeError::UndefinedVariable(name.to_string())),
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
                    let mut result = (a.add(b, &self.string_interner)).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?;
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
                Instruction::Call { arg_count } => {
                    let callee = self.peek_n(arg_count as usize).clone();
                    self.call(callee, arg_count as usize)?;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use env_logger::Env;

    use super::*;

    #[ctor::ctor]
    fn init() {
        env_logger::init_from_env(Env::new());
    }

    #[test]
    fn recursion() {
        let source = r#"
            fun fib(n) {
                if (n <= 1) return n;
                return fib(n - 2) + fib(n - 1);
            }

            print fib(10);
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "55\n");
    }

    #[test]
    fn string_interning() {
        let source = r#"
            print "a" == "a";

            var a = "a";
            var b = "b";
            var ab = a + b;
            print ab == "ab";
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "true\ntrue\n");
    }
}

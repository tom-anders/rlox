use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, HashSet},
    io::Write,
    ops::Neg,
    rc::Rc,
};

use bytecode::{
    instructions::{Instruction, Jump},
    value::{Function, NativeFun, RloxString, Value, ValueWithInterner}, string_interner::StringInterner,
};
use compiler::Compiler;
use errors::RloxErrors;
use itertools::Itertools;
use log::trace;

use self::stack::Stack;

mod stack;

#[derive(Debug)]
pub struct Vm {
    stack: Stack,
    string_interner: StringInterner,
    globals: HashMap<RloxString, Value>,
}

#[derive(Debug, thiserror::Error)]
pub enum InterpretError {
    #[error(transparent)]
    CompileError(#[from] RloxErrors),
    #[error("line {line}: {error}")]
    RuntimeError { line: usize, error: String },
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum RuntimeError<'a, 'b> {
    #[error("Invalid negate operant: {0}")]
    InvalidNegateOperant(ValueWithInterner<'a, 'b>),
    #[error("Invalid binary operants: ({0}, {1})")]
    InvalidBinaryOperants(ValueWithInterner<'a, 'b>, ValueWithInterner<'a, 'b>),
    #[error("Undefined variable: '{0}'")]
    UndefinedVariable(&'a str),
    #[error("{0} is not a function, can only call functions and classes")]
    NotAFunction(ValueWithInterner<'a, 'b>),
    #[error("Invalid argument count: expected {expected}, got {got}")]
    InvalidArgumentCount { expected: usize, got: usize },
}

pub type Result<T> = std::result::Result<T, InterpretError>;

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Stack::new(),
            string_interner: StringInterner::with_capacity(1024),
            globals: HashMap::new(),
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

    pub fn stack_trace(&self) -> String {
        self.stack.stack_trace(&self.string_interner)
    }

    fn runtime_error(&self, error: RuntimeError) -> InterpretError {
        InterpretError::RuntimeError {
            line: self.stack.current_line(),
            error: error.to_string(),
        }
    }

    pub fn run_source(&mut self, source: &str, stdout: &mut impl Write) -> Result<()> {
        let function = Rc::from(Compiler::new(source, &mut self.string_interner).compile()?);

        self.push(function.clone().into());
        self.call(function.into(), 0).unwrap();
        self.run(stdout)?;
        Ok(())
    }

    fn call(&mut self, value: Value, arg_count: usize) -> Result<()> {
        match value {
            Value::Function(function) => {
                if function.arity != arg_count {
                    return Err(self.runtime_error(RuntimeError::InvalidArgumentCount {
                        expected: function.arity,
                        got: arg_count,
                    }));
                }
                self.stack.push_frame(function);
                Ok(())
            }
            Value::NativeFun(native_fun) => {
                let args = self.stack.peek_n(arg_count);
                let result = native_fun.0(args.cloned().collect());
                self.stack.pop_n(arg_count);
                self.push(result);
                Ok(())
            }
            _ => Err(self.runtime_error(RuntimeError::NotAFunction(value.with_interner(&self.string_interner)))),
        }
    }

    fn define_native(&mut self, name: &str, native_fun: fn(Vec<Value>) -> Value) {
        self.globals
            .insert(RloxString::new(name, &mut self.string_interner), NativeFun(native_fun).into());
    }

    fn run(&mut self, stdout: &mut impl Write) -> Result<()> {
        loop {
            let bytes = &self.stack.frame_chunk().code()[self.stack.frame().ip()..];
            let op = Instruction::from_bytes(bytes);

            log::trace!("Stack: {}", self.stack.with_interner(&self.string_interner));
            log::trace!("Globals: {:?}", self.globals);
            log::trace!("Constants: {:?}", self.stack.frame_chunk().constants());
            log::trace!("{}", self.stack.frame_chunk().disassemble_instruction(self.stack.frame().ip(), &self.string_interner).0);

            self.stack.frame_mut().inc_ip(op.num_bytes());

            match op {
                Instruction::Return => {
                    let result = self.stack.pop();
                    self.stack.pop_frame();
                    if self.stack.frames().is_empty() {
                        return Ok(());
                    }
                    self.stack.push(result);
                }
                Instruction::Print => {
                    writeln!(stdout, "{}", self.stack.pop().with_interner(&self.string_interner)).unwrap();
                }
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::PopN(n) => {
                    self.stack.pop_n(n as usize);
                }
                Instruction::Constant { index } => {
                    let constant = self.stack.frame_chunk().constants().get(index as usize).unwrap();
                    trace!("Pushed constant: {:?}", constant);
                    self.push(constant.clone());
                }
                Instruction::DefineGlobal { constant_index } => {
                    let name = self
                        .stack
                        .frame_chunk()
                        .get_string_constant(constant_index)
                        .expect("Missing string constant for global");

                    trace!("Defining global: {:?} {:?}", name, self.stack.peek());
                    self.globals.insert(*name, self.stack.peek().clone());
                    self.stack.pop();
                }
                Instruction::SetGlobal { constant_index } => {
                    let name = self
                        .stack
                        .frame_chunk()
                        .get_string_constant(constant_index)
                        .expect("Missing string constant for global")
                        .clone();

                    let val = self.stack.peek().clone();
                    match self.globals.entry(name) {
                        Entry::Occupied(mut entry) => {
                            *entry.get_mut() = val;
                        }
                        Entry::Vacant(_) => {
                            return Err(self
                                .runtime_error(RuntimeError::UndefinedVariable(name.as_str(&self.string_interner))))
                        }
                    };
                }
                Instruction::GetGlobal { constant_index } => {
                    let name = self
                        .stack
                        .frame_chunk()
                        .get_string_constant(constant_index)
                        .expect("Missing string constant for global");

                    let value = self.globals.get(name).ok_or_else(|| {
                        self.runtime_error(RuntimeError::UndefinedVariable(name.as_str(&self.string_interner)))
                    })?;

                    self.push(value.clone());
                }
                Instruction::GetLocal { stack_slot } => {
                    self.push(self.stack.stack_at(stack_slot).clone());
                }
                Instruction::SetLocal { stack_slot } => {
                    *self.stack.stack_at_mut(stack_slot) = self.stack.peek().clone();
                }
                Instruction::Nil => self.push(Value::Nil),
                Instruction::True => self.push(Value::Boolean(true)),
                Instruction::False => self.push(Value::Boolean(false)),
                Instruction::Negate => {
                    let v = self.stack.pop();
                    let neg = v
                        .clone()
                        .neg()
                        .map_err(|v| self.runtime_error(RuntimeError::InvalidNegateOperant(v.with_interner(&self.string_interner))))?;
                    self.push(neg);
                }
                Instruction::Not => {
                    let v = self.stack.pop();
                    self.push(Value::Boolean(!v.is_truthy()));
                }
                Instruction::Equal => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.push(Value::Boolean(a == b));
                }
                Instruction::Less => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.push(a.less_than(b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a.with_interner(&self.string_interner), b.with_interner(&self.string_interner)))
                    })?);
                }
                Instruction::Greater => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.push(a.greater_than(b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a.with_interner(&self.string_interner), b.with_interner(&self.string_interner)))
                    })?);
                }
                Instruction::Add => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    let result = (a.add(b, &mut self.string_interner)).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a.with_interner(&self.string_interner), b.with_interner(&self.string_interner)))
                    })?;
                    self.push(result);
                }
                Instruction::Subtract => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.push((a - b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a.with_interner(&self.string_interner), b.with_interner(&self.string_interner)))
                    })?);
                }
                Instruction::Multiply => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.push((a * b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a.with_interner(&self.string_interner), b.with_interner(&self.string_interner)))
                    })?);
                }
                Instruction::Divide => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.push((a / b).map_err(|(a, b)| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a.with_interner(&self.string_interner), b.with_interner(&self.string_interner)))
                    })?);
                }
                Instruction::JumpIfFalse(Jump(jump)) => {
                    if self.stack.peek().is_falsey() {
                        self.stack.frame_mut().inc_ip(jump as usize);
                    }
                }
                Instruction::Jump(Jump(jump)) => {
                    self.stack.frame_mut().inc_ip(jump as usize);
                }
                Instruction::Loop(Jump(jump)) => {
                    self.stack.frame_mut().decr_ip(jump as usize);
                }
                Instruction::Call { arg_count } => {
                    let callee = self.stack.peek_n(arg_count as usize).next().unwrap();
                    self.call(callee.clone(), arg_count as usize)?;
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

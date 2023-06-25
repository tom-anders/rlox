use std::{
    collections::{hash_map::Entry, HashMap},
    io::Write, ops::Deref,
};

use compiler::Compiler;
use errors::RloxErrors;
use gc::{Object, Value, NativeFun, RloxString, Chunk, Closure, Heap, StringInterner};
use instructions::{Arity, Instruction, Jump};
use itertools::Itertools;
use log::trace;

use self::stack::Stack;

mod stack;

#[derive(Debug)]
pub struct Vm {
    stack: Stack,
    heap: Heap,
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

#[derive(Debug, Clone)]
pub enum RuntimeError<'a> {
    InvalidNegateOperant(Value),
    InvalidBinaryOperants(Value, Value),
    UndefinedVariable(&'a str),
    NotAFunction(Value),
    InvalidArgumentCount { expected: Arity, got: Arity },
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
            heap: Heap::with_capacity(1024),
        };
        vm.define_native("clock", |_| {
            let now = std::time::SystemTime::now();
            let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
            Value::Number(duration.as_secs_f64())
        });
        vm
    }

    fn push(&mut self, value: impl Into<Value>) {
        self.stack.push(value.into());
    }

    pub fn stack_trace(&self) -> String {
        self.stack.stack_trace(&self.string_interner)
    }

    fn runtime_error(&self, error: RuntimeError) -> InterpretError {
        InterpretError::RuntimeError {
            line: self.stack.current_line(),
            error: match error {
                RuntimeError::NotAFunction(v) => format!(
                    "Expected a function, got {}",
                    v.resolve(&self.string_interner)
                ),
                RuntimeError::InvalidArgumentCount { expected, got } => {
                    format!("Expected {} arguments, got {}", expected, got)
                }
                RuntimeError::InvalidNegateOperant(v) => format!(
                    "Expected a number, got {}",
                    v.resolve(&self.string_interner)
                ),
                RuntimeError::InvalidBinaryOperants(l, r) => format!(
                    "Expected two numbers, got {} and {}",
                    l.resolve(&self.string_interner),
                    r.resolve(&self.string_interner)
                ),
                RuntimeError::UndefinedVariable(name) => format!("Undefined variable {}", name),
            },
        }
    }

    pub fn run_source(&mut self, source: &str, stdout: &mut impl Write) -> Result<()> {
        let function =
            Compiler::new(source, &mut self.string_interner, &mut self.heap).compile()?;
        let function_ref = self.heap.alloc(function);

        let closure = Value::Object(self.heap.alloc(Closure::new(function_ref.unwrap_function())));
        self.push(closure);
        self.call(closure, Arity(0)).unwrap();
        self.run(stdout)?;
        Ok(())
    }

    fn call(&mut self, value: Value, arg_count: Arity) -> Result<()> {
        match value {
            Value::Object(obj) => match &*obj {
                Object::Closure(closure) => {
                    if closure.function().arity != arg_count {
                        return Err(self.runtime_error(RuntimeError::InvalidArgumentCount {
                            expected: closure.function().arity,
                            got: arg_count,
                        }));
                    }
                    self.stack.push_frame(obj.try_into().unwrap());
                    Ok(())
                }
                Object::NativeFun(native_fun) => {
                    let args =
                        self.stack.peek_n(arg_count.0 as usize);
                    let result = native_fun.0(args.cloned().collect());
                    self.stack.pop_n(arg_count.0 as usize);
                    self.push(result);
                    Ok(())
                }
                _ => Err(self.runtime_error(RuntimeError::NotAFunction(value))),
            }
            _ => Err(self.runtime_error(RuntimeError::NotAFunction(value))),
        }
    }

    fn define_native(&mut self, name: &str, native_fun: fn(Vec<Value>) -> Value) {
        let native_fun = self.heap.alloc(NativeFun(native_fun));
        self.globals.insert(
            RloxString::new(name, &mut self.string_interner),
            native_fun.into(),
        );
    }

    fn frame_chunk(&self) -> &Chunk {
        self.stack.frame_chunk()
    }

    fn run(&mut self, stdout: &mut impl Write) -> Result<()> {
        loop {
            let bytes = &self.frame_chunk().code()[self.stack.frame().ip()..];
            let op = Instruction::from_bytes(bytes);

            log::trace!("Stack: {}", self.stack.resolve(&self.string_interner));
            log::trace!(
                "Globals: {:?}",
                self.globals
                    .iter()
                    .map(|(name, value)| {
                        (
                            name.resolve(&self.string_interner),
                            value.resolve(&self.string_interner),
                        )
                    })
                    .collect_vec()
            );
            log::trace!("Constants: {:?}", self.frame_chunk().constants());
            log::trace!(
                "{}",
                self.frame_chunk()
                    .disassemble_instruction(
                        self.stack.frame().ip(),
                        &self.string_interner,
                    )
                    .0
            );

            self.stack.frame_mut().inc_ip(op.num_bytes());

            match op {
                Instruction::Return => {
                    let result = self.stack.pop();
                    let popped_frame = self.stack.pop_frame();
                    if self.stack.frames().is_empty() {
                        return Ok(());
                    }
                    self.stack.truncate_stack(popped_frame.base_slot());
                    self.stack.push(result);
                }
                Instruction::Print => writeln!(
                    stdout,
                    "{}",
                    self.stack.pop().resolve(&self.string_interner)
                )
                .unwrap(),
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::PopN(n) => {
                    self.stack.pop_n(n as usize);
                }
                Instruction::Constant { index } => {
                    let constant = self.frame_chunk().constants().get(index as usize).unwrap();
                    trace!("Pushed constant: {:?}", constant);
                    self.push(*constant);
                }
                Instruction::DefineGlobal { constant_index } => {
                    let name =
                        self.stack.frame_chunk().get_string_constant(constant_index);

                    trace!(
                        "Defining global: {:?} {:?}",
                        name.resolve(&self.string_interner),
                        self.stack.peek().resolve(&self.string_interner)
                    );
                    self.globals.insert(*name, *self.stack.peek());
                    self.stack.pop();
                }
                Instruction::SetGlobal { constant_index } => {
                    let name =
                        self.stack.frame_chunk().get_string_constant(constant_index);

                    let val = self.stack.peek();
                    match self.globals.entry(*name) {
                        Entry::Occupied(mut entry) => {
                            *entry.get_mut() = *val;
                        }
                        Entry::Vacant(_) => {
                            return Err(self.runtime_error(RuntimeError::UndefinedVariable(
                                name.resolve(&self.string_interner),
                            )))
                        }
                    };
                }
                Instruction::GetGlobal { constant_index } => {
                    let name =
                        self.stack.frame_chunk().get_string_constant(constant_index);

                    let value = self.globals.get(&name).ok_or_else(|| {
                        self.runtime_error(RuntimeError::UndefinedVariable(
                            name.resolve(&self.string_interner),
                        ))
                    })?;

                    self.push(*value);
                }
                Instruction::GetLocal { stack_slot } => {
                    self.push(*self.stack.stack_at(stack_slot));
                }
                Instruction::SetLocal { stack_slot } => {
                    *self.stack.stack_at_mut(stack_slot) = *self.stack.peek();
                }
                Instruction::Nil => self.push(Value::Nil),
                Instruction::True => self.push(Value::Boolean(true)),
                Instruction::False => self.push(Value::Boolean(false)),
                Instruction::Negate => {
                    let v = self.stack.pop();
                    let neg = v
                        .negate()
                        .ok_or_else(|| self.runtime_error(RuntimeError::InvalidNegateOperant(v)))?;
                    self.push(neg);
                }
                Instruction::Not => {
                    let v = self.stack.pop();
                    self.push(Value::Boolean(!v.is_truthy()));
                }
                Instruction::Equal => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.push(Value::Boolean(a.equals(&b)));
                }
                Instruction::Less => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    let res = a.less_than(&b).ok_or_else(|| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?;
                    self.push(res);
                }
                Instruction::Greater => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    let res = a.greater_than(&b).ok_or_else(|| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?;
                    self.push(res);
                }
                Instruction::Add => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    let res = a.add(&b, &mut self.heap, &mut self.string_interner).ok_or_else(|| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?;
                    self.push(res);
                }
                Instruction::Subtract => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    let res = a.subtract(&b).ok_or_else(|| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?;
                    self.push(res);
                }
                Instruction::Multiply => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    let res = a.multiply(&b).ok_or_else(|| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?;
                    self.push(res);
                }
                Instruction::Divide => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    let res = a.divide(&b).ok_or_else(|| {
                        self.runtime_error(RuntimeError::InvalidBinaryOperants(a, b))
                    })?;
                    self.push(res);
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
                    let callee = self.stack.peek_n(arg_count.0 as usize).next().unwrap();
                    self.call(*callee, arg_count)?;
                }
                Instruction::Closure { constant_index } => {
                    let function = self.stack.frame_chunk().get_function_constant(constant_index);
                    let closure = self.heap.alloc(Closure::new(function));
                    self.push(closure);
                }
                Instruction::SetUpvalue { upvalue_index } => {
                    todo!()
                }
                Instruction::GetUpvalue { upvalue_index } => {
                    todo!()
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

    #[test]
    fn open_upvalues() {
        let source = r#"
            fun outer() {
              var x = "outside";
              fun inner() {
                print x;
              }
              inner();
            }
            outer();
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "outside\n");
    }

    #[test]
    fn closed_upvalues() {
        let source = r#"
            fun outer() {
              var x = "outside";
              fun inner() {
                print x;
              }

              return inner;
            }

            var closure = outer();
            closure();
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "outside\n");
    }
}

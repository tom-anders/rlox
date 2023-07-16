use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    io::Write,
    ops::Deref,
};

use compiler::Compiler;
use errors::RloxErrors;
use gc::{
    Chunk, Class, Closure, GarbageCollector, Heap, Instance, NativeFun, Object, ObjectRef,
    RloxString, StringInterner, TypedObjectRef, Upvalue, UpvalueRef, Value,
};
use instructions::{Arity, Instruction, Jump};
use itertools::Itertools;
use log::trace;

use self::stack::Stack;

mod stack;

#[derive(Debug)]
pub struct Vm {
    stack: Stack,
    heap: Heap,
    gc: GarbageCollector,
    string_interner: StringInterner,
    globals: HashMap<RloxString, Value>,
    // TODO clox uses a linked list here, check if this is actually more performant
    open_upvalues: Vec<UpvalueRef>,
}

#[derive(Debug, PartialEq, thiserror::Error)]
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
    UndefinedProperty(&'a str),
    NotAFunction(Value),
    InvalidArgumentCount { expected: Arity, got: Arity },
    InvalidPropertyAccess,
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
            gc: GarbageCollector::default(),
            open_upvalues: Vec::new(),
        };
        vm.define_native("clock", |_| {
            let now = std::time::SystemTime::now();
            let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
            Value::Number(duration.as_secs_f64())
        });
        vm
    }

    fn alloc<T>(&mut self, object: T) -> TypedObjectRef<T>
    where
        T: Into<Object> + Clone,
    {
        log::debug!("Allocating {:?}", object.clone().into().resolve(&self.string_interner));

        if self.gc.gc_needed(&mut self.heap) {
            for value in self.stack.iter_mut() {
                self.gc.mark_value(value);
            }

            for value in self.globals.values_mut() {
                self.gc.mark_value(value);
            }

            for frame in self.stack.frames_mut() {
                self.gc.mark_ref(frame.closure_mut());
            }

            for upvalue in &mut self.open_upvalues {
                self.gc.mark_ref(upvalue);
            }

            self.gc.collect_garbage(&mut self.heap, &self.string_interner);
        }

        self.heap.alloc(object.into()).into()
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
                RuntimeError::NotAFunction(v) => {
                    format!("Expected a function, got {}", v.resolve(&self.string_interner))
                }
                RuntimeError::InvalidArgumentCount { expected, got } => {
                    format!("Expected {} arguments, got {}", expected, got)
                }
                RuntimeError::InvalidNegateOperant(v) => {
                    format!("Expected a number, got {}", v.resolve(&self.string_interner))
                }
                RuntimeError::InvalidBinaryOperants(l, r) => format!(
                    "Expected two numbers, got {} and {}",
                    l.resolve(&self.string_interner),
                    r.resolve(&self.string_interner)
                ),
                RuntimeError::UndefinedVariable(name) => format!("Undefined variable '{}'.", name),
                RuntimeError::UndefinedProperty(name) => format!("Undefined property '{}'.", name),
                RuntimeError::InvalidPropertyAccess => {
                    "Only instances have properties.".to_string()
                }
            },
        }
    }

    pub fn run_source(&mut self, source: &str, stdout: &mut impl Write) -> Result<()> {
        let closure = Value::Object(
            Compiler::new(source, &mut self.string_interner, &mut self.heap).compile()?.into(),
        );

        self.push(closure.clone());
        self.call(closure, Arity(0)).unwrap();
        self.run(stdout)?;
        Ok(())
    }

    fn call(&mut self, value: Value, arg_count: Arity) -> Result<()> {
        match &value {
            Value::Object(obj) => match obj.deref() {
                Object::Closure(closure) => {
                    if closure.function().arity != arg_count {
                        return Err(self.runtime_error(RuntimeError::InvalidArgumentCount {
                            expected: closure.function().arity,
                            got: arg_count,
                        }));
                    }
                    self.stack.push_frame(obj.clone().try_into().unwrap());
                    Ok(())
                }
                Object::Class(_) => {
                    // TODO pass fields
                    let instance =
                        self.alloc(Instance::new(obj.clone().unwrap_class(), HashMap::new()));
                    *self.stack.stack_at_mut(self.stack.len() - 1 - arg_count.0) =
                        Value::Object(instance.into());
                    Ok(())
                }
                Object::NativeFun(native_fun) => {
                    let args = self.stack.peek_n(arg_count.0 as usize);
                    let result = native_fun.0(args.cloned().collect());
                    self.stack.pop_n(arg_count.0 as usize);
                    self.push(result);
                    Ok(())
                }
                _ => Err(self.runtime_error(RuntimeError::NotAFunction(value))),
            },
            _ => Err(self.runtime_error(RuntimeError::NotAFunction(value))),
        }
    }

    fn define_native(&mut self, name: &str, native_fun: fn(Vec<Value>) -> Value) {
        let native_fun = self.alloc(NativeFun(native_fun));
        self.globals.insert(
            RloxString::new(name, &mut self.string_interner),
            Value::Object(native_fun.into()),
        );
    }

    fn frame_chunk(&self) -> &Chunk {
        self.stack.frame_chunk()
    }

    fn run(&mut self, stdout: &mut impl Write) -> Result<()> {
        loop {
            let bytes = &self.frame_chunk().code()[self.stack.frame().ip()..];
            let op = Instruction::from_bytes(bytes);

            log::debug!("Stack: {}", self.stack.resolve(&self.string_interner));
            log::trace!(
                "Globals: {:?}",
                self.globals
                    .iter()
                    .map(|(name, value)| {
                        (name.resolve(&self.string_interner), value.resolve(&self.string_interner))
                    })
                    .collect_vec()
            );
            log::trace!("Constants: {:?}", self.frame_chunk().constants());
            log::trace!(
                "{}",
                self.frame_chunk()
                    .disassemble_instruction(self.stack.frame().ip(), &self.string_interner,)
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
                    self.open_upvalues.retain_mut(|open_upvalue| {
                        let retain =
                            open_upvalue.stack_slot().unwrap() < popped_frame.base_slot() as u8;
                        if !retain {
                            unsafe {
                                let value = self
                                    .stack
                                    .global_stack_at(open_upvalue.stack_slot().unwrap())
                                    .clone();
                                *open_upvalue.deref_mut() = Upvalue::Closed(value)
                            }
                        }
                        retain
                    });
                    self.stack.truncate_stack(popped_frame.base_slot());
                    self.stack.push(result);
                }
                Instruction::Print => {
                    writeln!(stdout, "{}", self.stack.pop().resolve(&self.string_interner)).unwrap()
                }
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::PopN(n) => {
                    self.stack.pop_n(n as usize);
                }
                Instruction::CloseUpvalue => {
                    let index = self
                        .open_upvalues
                        .iter()
                        .position(|open_upvalue| {
                            open_upvalue.stack_slot().unwrap() == self.stack.len() - 1
                        })
                        .expect("Should have an open upvalue at the top of the stack");

                    let open_upvalue = self.open_upvalues.get_mut(index).unwrap();
                    unsafe {
                        let value =
                            self.stack.global_stack_at(open_upvalue.stack_slot().unwrap()).clone();
                        *open_upvalue.deref_mut() = Upvalue::Closed(value)
                    }

                    self.open_upvalues.swap_remove(index);
                }
                Instruction::Constant { index } => {
                    let constant = self.frame_chunk().constants().get(index as usize).unwrap();
                    trace!("Pushed constant: {:?}", constant);
                    self.push(constant.clone());
                }
                Instruction::DefineGlobal { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    trace!(
                        "Defining global: {:?} {:?}",
                        name.resolve(&self.string_interner),
                        self.stack.peek().resolve(&self.string_interner)
                    );
                    self.globals.insert(*name, self.stack.peek().clone());
                    self.stack.pop();
                }
                Instruction::SetGlobal { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    let val = self.stack.peek();
                    match self.globals.entry(*name) {
                        Entry::Occupied(mut entry) => {
                            *entry.get_mut() = val.clone();
                        }
                        Entry::Vacant(_) => {
                            return Err(self.runtime_error(RuntimeError::UndefinedVariable(
                                name.resolve(&self.string_interner),
                            )))
                        }
                    };
                }
                Instruction::GetGlobal { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    let value = self.globals.get(&name).ok_or_else(|| {
                        self.runtime_error(RuntimeError::UndefinedVariable(
                            name.resolve(&self.string_interner),
                        ))
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
                    let res =
                        a.add(&b, &mut self.heap, &mut self.string_interner).ok_or_else(|| {
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
                    self.call(callee.clone(), arg_count)?;
                }
                Instruction::Closure { constant_index, upvalue_count } => {
                    let function = self.stack.frame_chunk().get_function_constant(constant_index);

                    let ip = self.stack.frame().ip();
                    let upvalue_bytes = self.frame_chunk().code()
                        [ip..ip + 2 * upvalue_count as usize]
                        .iter()
                        .copied()
                        .collect_vec();

                    let upvalues = upvalue_bytes
                        .chunks(2)
                        .into_iter()
                        .map(|chunk| {
                            let (is_local, index) = (chunk[0], chunk[1]);
                            if is_local == 0 {
                                self.capture_upvalue(
                                    self.stack.frame().base_slot() + index as usize,
                                )
                            } else {
                                self.stack.frame().closure().upvalues()[index as usize].clone()
                            }
                        })
                        .collect_vec();

                    self.stack.frame_mut().inc_ip(upvalue_bytes.len());

                    let closure = self.alloc(Closure::new(function, upvalues));
                    self.push(ObjectRef::from(closure));
                }
                Instruction::GetUpvalue { upvalue_index } => self.push(
                    match self
                        .stack
                        .frame()
                        .closure()
                        .upvalues()
                        .get(upvalue_index as usize)
                        .expect("invalid upvalue")
                        .deref()
                    {
                        Upvalue::Local { stack_slot } => {
                            self.stack.global_stack_at(*stack_slot).clone()
                        }
                        Upvalue::Closed(value) => value.clone(),
                    },
                ),
                Instruction::SetUpvalue { upvalue_index } => {
                    let mut upvalue_ref = self
                        .stack
                        .frame()
                        .closure()
                        .upvalues()
                        .get(upvalue_index as usize)
                        .expect("invalid upvalue")
                        .clone();

                    let stack_top = self.stack.peek().clone();
                    unsafe {
                        // SAFETY: The VM does not hold on to upvalue references after any
                        // instruction, so we can safely get a mutable reference here.
                        match upvalue_ref.deref_mut() {
                            Upvalue::Local { stack_slot } => {
                                *self.stack.global_stack_at_mut(*stack_slot) = stack_top;
                            }
                            Upvalue::Closed(value) => {
                                *value = stack_top;
                            }
                        }
                    }
                }
                Instruction::Class { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);
                    let class = self.alloc(Class::new(*name));
                    self.push(Value::Object(class.into()));
                }
                Instruction::GetProperty { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    let instance: &Instance = self
                        .stack
                        .peek()
                        .try_into()
                        .map_err(|_| self.runtime_error(RuntimeError::InvalidPropertyAccess))?;

                    let field = instance
                        .field(name.deref())
                        .ok_or_else(|| {
                            self.runtime_error(RuntimeError::UndefinedProperty(
                                name.resolve(&self.string_interner),
                            ))
                        })?
                        .clone();

                    log::debug!(
                        "get property: {} -> {}",
                        name.resolve(&self.string_interner),
                        field.resolve(&self.string_interner)
                    );
                    self.stack.pop();
                    self.push(field.clone());
                }
                Instruction::SetProperty { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    let mut object: ObjectRef =
                        self.stack.peek_n(1).next().unwrap().clone().try_into().ok().ok_or_else(
                            || self.runtime_error(RuntimeError::InvalidPropertyAccess),
                        )?;
                    let object = unsafe { object.deref_mut() };

                    if let Object::Instance(instance) = object {
                        let value = self.stack.pop();
                        instance
                            .field_mut(name.deref())
                            .and_modify(|field| *field = value.clone())
                            .or_insert_with(|| value.clone());
                        self.stack.pop();
                        self.stack.push(value);
                    } else {
                        return Err(self.runtime_error(RuntimeError::InvalidPropertyAccess));
                    }
                }
            }
        }
    }

    fn capture_upvalue(&mut self, stack_slot: usize) -> UpvalueRef {
        if let Some(upvalue) = self.open_upvalues.iter().find(|upvalue| {
            upvalue.stack_slot().expect("Should only contain open upvalues") == stack_slot as u8
        }) {
            return upvalue.clone();
        }

        let upvalue = self.alloc(Upvalue::Local { stack_slot: stack_slot as u8 });
        self.open_upvalues.push(upvalue);

        self.open_upvalues.last().unwrap().clone()
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

              // this tests that closures capture variables, not values
              fun mutate_x() {
                  x = "mutated";
              }
              mutate_x();

              return inner;
            }

            var closure = outer();
            closure();
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "mutated\n");
    }

    #[test]
    fn classes() {
        let source = r#"
            class Pair {}

            var pair = Pair();
            pair.first = 1;
            pair.second = 2;
            print pair;
            print pair.first + pair.second;
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap().lines().collect_vec(),
            vec!["Pair instance", "3"]
        );
    }

    #[test]
    fn invalid_field() {
        let source = r#" class Foo {}
                               var foo = Foo();
                               print foo.bar;
                            "#;
        let mut output = Vec::new();
        assert_eq!(
            Vm::new().run_source(source, &mut output).unwrap_err(),
            InterpretError::RuntimeError {
                line: 3,
                error: "Undefined property 'bar'.".to_string()
            }
        )
    }
}

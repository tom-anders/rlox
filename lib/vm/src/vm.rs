use std::{io::Write, ops::Deref};

use compiler::{Compiler, CompilerErrors};
use gc::{
    BoundMethod, Chunk, Class, Closure, GarbageCollector, Heap, Instance, InstanceRef, NativeFun,
    Object, ObjectRef, TypedObjectRef, Upvalue, UpvalueRef, Value, ClassRef,
};
use instructions::{Arity, Instruction, Jump};
use itertools::Itertools;
use log::trace;
use strings::{string_interner::{StringInterner, InternedString}, table::StringTable};

use self::stack::Stack;

mod stack;

#[derive(Debug)]
pub struct Vm {
    stack: Stack,
    heap: Heap,
    gc: GarbageCollector,
    string_interner: StringInterner,
    init_string: InternedString,
    globals: StringTable<Value>,
    // TODO clox uses a linked list here, check if this is actually more performant
    open_upvalues: Vec<UpvalueRef>,
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum InterpretError {
    #[error(transparent)]
    CompileError(#[from] CompilerErrors),
    #[error("line {line}: {error}")]
    RuntimeError { line: usize, error: RuntimeError },
}

#[derive(thiserror::Error, Debug, Clone, PartialEq)]
pub enum RuntimeError {
    #[error("Expected a number, got {0}")]
    InvalidNegateOperant(Value),
    #[error("Expected two numbers, got {0} and {1}")]
    InvalidBinaryOperants(Value, Value),
    #[error("Undefined variable '{0}'.")]
    UndefinedVariable(String),
    #[error("Undefined property '{0}'.")]
    UndefinedProperty(String),
    #[error("Expected a function, got {0}")]
    NotAFunction(Value),
    #[error("Expected {expected} arguments, got {got}")]
    InvalidArgumentCount { expected: Arity, got: Arity },
    #[error("Only instances have properties.")]
    InvalidPropertyAccess,
    #[error("Only instances have methods.")]
    OnlyInstancesHaveMethods,
    #[error("Superclass must be a class.")]
    SuperclassMustBeAClass,
}

pub type Result<T> = std::result::Result<T, InterpretError>;

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        let mut string_interner = StringInterner::with_capacity(1024);
        let init_string = string_interner.intern("init");
        let mut vm = Self {
            stack: Stack::new(),
            string_interner,
            init_string,
            globals: StringTable::default(),
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
        log::debug!("Allocating {:?}", object.clone().into());

        if self.gc.gc_needed(&mut self.heap) {
            for value in self.stack.iter_mut() {
                self.gc.mark_value(value);
            }

            for value in self.globals.values_mut() {
                self.gc.mark_value(value);
            }

            for frame in self.stack.iter_frames_mut() {
                self.gc.mark_ref(frame.closure_mut());
            }

            for upvalue in &mut self.open_upvalues {
                self.gc.mark_ref(upvalue);
            }

            self.gc.collect_garbage(&mut self.heap);
        }

        self.heap.alloc(object.into()).into()
    }

    fn push(&mut self, value: impl Into<Value>) {
        self.stack.push(value.into());
    }

    pub fn stack_trace(&self) -> String {
        self.stack.stack_trace()
    }

    fn runtime_error(&self, error: RuntimeError) -> InterpretError {
        InterpretError::RuntimeError { line: self.stack.current_line(), error }
    }

    pub fn run_source(&mut self, source: &str, stdout: &mut impl Write) -> Result<()> {
        // If the previous call to run_source failed, the stack will still contain values/frames.
        // This is needed so that we can show the strack trace.
        // But once we try to execute more code, we need to clear the stack.
        self.stack.clear();

        let closure = Value::Object(
            Compiler::new(source, &mut self.string_interner, &mut self.heap).compile()?.into(),
        );

        self.push(closure.clone());
        self.call(closure, Arity(0)).unwrap();
        self.run(stdout)?;
        Ok(())
    }

    fn call(&mut self, value: Value, arg_count: Arity) -> Result<()> {
        match value {
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
                    let instance =
                        self.alloc(Instance::new(obj.clone().unwrap_class(), StringTable::new()));
                    *self.stack.global_stack_at_mut(self.stack.len() - 1 - arg_count.0) =
                        instance.clone().into();

                    match instance.class().get_method(&self.init_string) {
                        Some(init) => self.call(init.into(), arg_count),
                        None => {
                            if arg_count.0 == 0 {
                                Ok(())
                            } else {
                                // Class has no explicit init method, but arguments where passed anyway
                                Err(self.runtime_error(RuntimeError::InvalidArgumentCount {
                                    expected: Arity(0),
                                    got: arg_count,
                                }))
                            }
                        }
                    }
                }
                Object::NativeFun(native_fun) => {
                    let args = self.stack.iter().rev().take(arg_count.0 as usize).cloned();
                    let result = native_fun.0(args.collect());
                    self.stack.pop_n(arg_count.0 as usize);
                    self.push(result);
                    Ok(())
                }
                Object::BoundMethod(bound_method) => {
                    *self.stack.global_stack_at_mut(self.stack.len() - 1 - arg_count.0) =
                        bound_method.receiver().into();
                    self.call(bound_method.method().into(), arg_count)
                }
                _ => Err(self.runtime_error(RuntimeError::NotAFunction(obj.into()))),
            },
            _ => Err(self.runtime_error(RuntimeError::NotAFunction(value))),
        }
    }

    fn invoke_from_class(&mut self, class: &Class, name: InternedString, arg_count: Arity) -> Result<()> {
        match class.get_method(&name) {
            Some(method) => self.call(method.into(), arg_count),
            None => Err(self.runtime_error(RuntimeError::UndefinedProperty(name.to_string()))),
        }
    }

    fn define_native(&mut self, name: &str, native_fun: fn(Vec<Value>) -> Value) {
        let native_fun = self.alloc(NativeFun(native_fun));
        self.globals.insert(self.string_interner.intern(name), native_fun.into());
    }

    fn frame_chunk(&self) -> &Chunk {
        self.stack.frame_chunk()
    }

    fn run(&mut self, stdout: &mut impl Write) -> Result<()> {
        loop {
            let bytes = &self.frame_chunk().code()[self.stack.frame().ip()..];
            let op = Instruction::from_bytes(bytes);

            log::debug!("Stack: {:?}", self.stack);
            log::trace!("Globals: {:?}", self.globals);
            log::trace!("Constants: {:?}", self.frame_chunk().constants());
            log::trace!(
                "{}",
                self.frame_chunk().disassemble_instruction(self.stack.frame().ip()).0
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
                Instruction::Print => writeln!(stdout, "{}", self.stack.pop()).unwrap(),
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

                    trace!("Defining global: {:?} {:?}", name, self.stack.peek());
                    self.globals.insert(name, self.stack.peek().clone());
                    self.stack.pop();
                }
                Instruction::SetGlobal { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    let val = self.stack.peek();
                    match self.globals.get_mut(name) {
                        Some(entry) => *entry = val.clone(),
                        None => {
                            return Err(self
                                .runtime_error(RuntimeError::UndefinedVariable(name.to_string())))
                        }
                    };
                }
                Instruction::GetGlobal { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    let value = self.globals.get(name).ok_or_else(|| {
                        self.runtime_error(RuntimeError::UndefinedVariable(name.to_string()))
                    })?;

                    self.push(value.clone());
                }
                Instruction::GetLocal { stack_slot } => {
                    self.push(self.stack.frame_stack_at(stack_slot).clone());
                }
                Instruction::SetLocal { stack_slot } => {
                    *self.stack.frame_stack_at_mut(stack_slot) = self.stack.peek().clone();
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
                    let callee = self.stack.value_stack().peek_nth(arg_count.0 as usize);
                    self.call(callee.clone(), arg_count)?;
                }
                Instruction::Invoke { constant_index, arg_count } => {
                    let receiver = self.stack.value_stack().peek_nth(arg_count.0 as usize).clone();
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);
                    
                    match receiver.as_instance() {
                        Some(instance) => {
                            if let Some(field) = instance.field(&name) {
                                self.call(field.clone(), arg_count)?
                            } else {
                                self.invoke_from_class(instance.class(), name, arg_count)?
                            }
                        }
                        None => return Err(self.runtime_error(RuntimeError::OnlyInstancesHaveMethods)),
                    }
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
                    let class = self.alloc(Class::new(name));
                    self.push(class);
                }
                Instruction::Inherit => {
                    let mut subclass = self.stack.pop().unwrap_object().unwrap_class();
                    match self.stack.value_stack().peek().as_class() {
                        Some(superclass) => {
                            // SAFETY: The VM never holds on to any references to the class for longer than a single instruction,
                            // so at this point we can be sure to have an exclusive reference to the class.
                            unsafe { 
                                subclass.deref_mut().set_methods(superclass.methods().clone()) 
                            } 
                        }
                        None => return Err(self.runtime_error(RuntimeError::SuperclassMustBeAClass)),
                    }
                }
                Instruction::GetProperty { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    let instance: InstanceRef = self.stack.peek().clone().unwrap_object().into();

                    if let Some(field) = instance.field(&name).cloned() {
                        log::debug!("get property: {} -> {}", name.deref(), field);
                        self.stack.pop();
                        self.push(field.clone());
                    } else if let Some(method) = instance.class().get_method(&name) {
                        let bound_method =
                            self.alloc(BoundMethod::new(instance.clone(), method.clone()));
                        self.stack.pop();
                        self.push(bound_method);
                    } else {
                        return Err(
                            self.runtime_error(RuntimeError::UndefinedProperty(name.to_string()))
                        );
                    }
                }
                Instruction::SetProperty { constant_index } => {
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);

                    let mut object: ObjectRef =
                        self.stack.value_stack().peek_nth(1).clone().try_into().ok().ok_or_else(
                            || self.runtime_error(RuntimeError::InvalidPropertyAccess),
                        )?;
                    let object = unsafe { object.deref_mut() };

                    if let Object::Instance(instance) = object {
                        let value = self.stack.pop();
                        instance.set_field(&name, value.clone());
                        self.stack.pop();
                        self.stack.push(value);
                    } else {
                        return Err(self.runtime_error(RuntimeError::InvalidPropertyAccess));
                    }
                }
                Instruction::Method { constant_index } => {
                    let (method, class) =
                        self.stack.value_stack().iter().rev().take(2).collect_tuple().unwrap();

                    let mut class = class.clone().unwrap_object().unwrap_class();
                    let name = self.stack.frame_chunk().get_string_constant(constant_index);
                    unsafe {
                        class
                            .deref_mut()
                            .add_method(name, method.clone().unwrap_object().unwrap_closure())
                    };
                    self.stack.pop();
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
    use compiler::{CompilerError, CompilerErrorType};
    use cursor::{Col, Line};
    use env_logger::Env;
    use pretty_assertions::assert_eq;

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
    fn bound_methods() {
        let source = r#"
            class Scone {
                topping(first, second) {
                    print "scone with " + first + " and " + second;
                }
            }

            var scone = Scone();
            scone.topping("berries", "cream");
            var topping = scone.topping;
            topping("berries", "cream");
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "scone with berries and cream\nscone with berries and cream\n"
        );
    }

    #[test]
    fn classes() {
        let source = r#"
            class Pair {
                init(first, second) {
                    this.first_ = first;
                    this.second_ = second;
                }
                first() { return this.first_; }
                second() { return this.second_; }
            }

            var pair = Pair(1, 2);
            print pair;
            print pair.first_ + pair.second_;
            print pair.first() + pair.second();
            print pair.first() == pair.first_;
        "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap().lines().collect_vec(),
            vec!["Pair instance", "3", "3", "true"]
        );
    }

    #[test]
    fn fields_shadow_methods() {
        let source = r#"
            class Foo { 
                bar() { print "method"; }
            }

            var foo = Foo();
            foo.bar = 123;
            print foo.bar;
        "#;
        let mut output = Vec::new();
        let mut vm = Vm::new();
        vm.run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output.clone()).unwrap().lines().collect_vec(), vec!["123"]);

        assert_eq!(
            vm.run_source("print foo.bar();", &mut output),
            Err(InterpretError::RuntimeError {
                line: 1,
                error: RuntimeError::NotAFunction(Value::Number(123.0)),
            })
        );
    }

    #[test]
    fn resolve_this_in_nested() {
        let source = r#" 
            class Nested {
              method() {
                fun function() {
                  print this;
                }

                function();
              }
            }

            Nested().method();
                            "#;
        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap().lines().collect_vec(),
            vec!["Nested instance"]
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
                error: RuntimeError::UndefinedProperty("bar".to_string())
            }
        )
    }

    #[test]
    fn this_outside_class() {
        let source = r#"this.foo = "bar"; "#;
        let mut output = Vec::new();
        assert_eq!(
            Vm::new().run_source(source, &mut output).unwrap_err(),
            InterpretError::CompileError(CompilerErrors(vec![CompilerError::new(
                CompilerErrorType::ThisOutsideClass,
                Line(1),
                Col(1),
            )]))
        )
    }

    #[test]
    fn class_init() {
        let source = r#"
            class Foo { 
                init(foo) {}
            }
            class Bar {}
        "#;
        let mut output = Vec::new();

        let mut vm = Vm::new();
        vm.run_source(source, &mut output).unwrap();

        output.clear();
        vm.run_source("var foo = Foo(123); print foo;", &mut output).unwrap();
        assert_eq!(String::from_utf8(output.clone()).unwrap().lines().collect_vec(), vec!["Foo instance"]);

        assert_eq!(vm.run_source("var foo = Foo();", &mut output).unwrap_err(), InterpretError::RuntimeError {
            line: 1,
            error: RuntimeError::InvalidArgumentCount { expected: Arity(1), got: Arity(0) }
        });

        assert_eq!(vm.run_source("foo = Foo(123, 456);", &mut output).unwrap_err(), InterpretError::RuntimeError {
            line: 1,
            error: RuntimeError::InvalidArgumentCount { expected: Arity(1), got: Arity(2) }
        });

        assert_eq!(vm.run_source("var bar = Bar(123);", &mut output).unwrap_err(), InterpretError::RuntimeError {
            line: 1,
            error: RuntimeError::InvalidArgumentCount { expected: Arity(0), got: Arity(1) }
        });
    }

    #[test]
    fn initializer_cannot_return_value() {
        let source = r#"
            class Foo { 
                init(foo) { 
                    return 1; 
                }
            }
        "#;
        let mut output = Vec::new();

        assert_eq!(Vm::new().run_source(source, &mut output).unwrap_err(), InterpretError::CompileError(
                CompilerErrors(vec![CompilerError::new(
                    CompilerErrorType::InitializerCannotReturn,
                    Line(4),
                    Col(29),
                    )])
                ));
    }

    #[test]
    fn inheritance() {
        let source = r#"
            class Foo { 
                method() { print "foo"; }
            }
            class Bar < Foo {}
            class Baz < Bar {}
            var baz = Baz();
            baz.method();
        "#;

        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output).unwrap().lines().collect_vec(), vec!["foo"]);
    }

    #[test]
    fn call_bound_method_inside_class() {
        let source = r#"
            class Foo { 
                foo() { print "foo"; }
                bar() {
                    var bar = this.foo;
                    bar();
                    return bar;
                }
            }
            var bar = Foo().bar();
            bar();
        "#;

        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output).unwrap().lines().collect_vec(), vec!["foo", "foo"]);
    }

    #[test]
    fn class_constructor_inside_local_scope() {
        let source = r#"
            class Foo {
                init() {
                    print "foo";
                }
            }
            fun bar() {
                var foo = Foo();
            }
            bar();
        "#;

        let mut output = Vec::new();
        Vm::new().run_source(source, &mut output).unwrap();
        assert_eq!(String::from_utf8(output).unwrap().lines().collect_vec(), vec!["foo"]);
    }
}

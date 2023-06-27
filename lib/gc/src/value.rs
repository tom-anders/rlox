use std::{ops::Deref, unreachable};

mod rlox_string;

pub use rlox_string::RloxString;

use crate::{string_interner::StringInterner, ObjectRef, Heap};

mod object;
pub use object::*;

mod function;
pub use function::*;

mod closure;
pub use closure::*;

mod upvalue;
pub use upvalue::*;

#[derive(Clone, Copy, Debug, PartialEq, derive_more::From, derive_more::TryInto, derive_more::Unwrap)]
#[try_into(owned, ref, ref_mut)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Object(ObjectRef),
}

impl Value {
    pub(crate) fn mark_reachable(&mut self) -> &mut Self {
        if let Value::Object(object) = self {
            object.mark_reachable()
        }
        self
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    pub fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            // Dereference first, since due to string two different ObjectRefs may actually refer
            // to the same string.
            (Value::Object(a), Value::Object(b)) => a.deref() == b.deref(),
            (a, b) => a == b,
        }
    }

    pub fn less_than(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Boolean(a < b)),
            _ => None,
        }
    }

    pub fn greater_than(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Boolean(a > b)),
            _ => None,
        }
    }

    pub fn negate(&self) -> Option<Value> {
        match self {
            Value::Number(n) => Some(Value::Number(-n)),
            _ => None,
        }
    }

    pub fn add(&self, other: &Value, heap: &mut Heap, interner: &mut StringInterner) -> Option<Value> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a + b)),
            (Value::Object(a), Value::Object(b)) => match (a.deref(), b.deref()) {
                (Object::String(a), Object::String(b)) => {
                    let res = format!("{}{}", a.resolve(interner), b.resolve(interner));
                    Some(Value::Object(heap.alloc(Object::String(RloxString::new(&res, interner))).into()))
                }
                _ => None,
            }
            _ => None,
        }
    }

    pub fn subtract(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a - b)),
            _ => None,
        }
    }

    pub fn multiply(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a * b)),
            _ => None,
        }
    }

    pub fn divide(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a / b)),
            _ => None,
        }
    }
}

impl Value {
    pub fn resolve<'a, 'b>(&'a self, interner: &'b StringInterner) -> ValueWithInterner<'a, 'b> {
        ValueWithInterner(self, interner)
    }
}

#[derive(Clone)]
pub struct ValueWithInterner<'a, 'b>(&'a Value, &'b StringInterner);

impl std::fmt::Debug for ValueWithInterner<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueWithInterner(Value::Number(n), _) => write!(f, "Number({})", n),
            ValueWithInterner(Value::Boolean(b), _) => write!(f, "Boolean({})", b),
            ValueWithInterner(Value::Nil, _) => write!(f, "Nil"),
            ValueWithInterner(Value::Object(o), _) => write!(f, "{:?}", o.resolve(self.1)),
        }
    }
}

impl std::fmt::Display for ValueWithInterner<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueWithInterner(Value::Number(n), _) => write!(f, "{}", n),
            ValueWithInterner(Value::Boolean(b), _) => write!(f, "{}", b),
            ValueWithInterner(Value::Nil, _) => write!(f, "nil"),
            ValueWithInterner(Value::Object(o), _) => match o.deref() {
                Object::String(s) => write!(f, "{}", s.resolve(self.1)),
                Object::Function(fun) => write!(f, "<fn {}>", fun.name.resolve(self.1)),
                Object::Closure(closure) => write!(f, "<fn {}>", closure.function().name.resolve(self.1)),
                Object::NativeFun(native_fn) => write!(f, "<native fn {:?}>", native_fn),
                Object::Upvalue(_) => unreachable!("Should not be able to print an upvalue"),
            }
        }
    }
}

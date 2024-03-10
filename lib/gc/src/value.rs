use std::ops::Deref;

use crate::{Heap, ObjectRef};

mod object;
pub use object::*;

mod function;
pub use function::*;

mod closure;
pub use closure::*;

mod upvalue;
use strings::string_interner::{StringInterner, InternedString};
pub use upvalue::*;

mod class;
pub use class::*;

mod instance;
pub use instance::*;

mod bound_method;
pub use bound_method::*;

#[derive(
    Clone,
    PartialEq,
    derive_more::From,
    derive_more::TryInto,
    derive_more::Unwrap,
    derive_more::Display,
)]
#[try_into(owned, ref, ref_mut)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    #[display(fmt = "nil")]
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
            // Dereference first, since due to string interning two different ObjectRefs may actually refer
            // to the same string.
            (Value::Object(a), Value::Object(b)) => a.deref() == b.deref(),
            (a, b) => a == b,
        }
    }

    pub fn equals_string(&self, s: &InternedString) -> bool {
        match self {
            Value::Object(o) => match o.deref() {
                Object::String(string) => string == s,
                _ => false,
            },
            _ => false,
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

    pub fn add(
        &self,
        other: &Value,
        heap: &mut Heap,
        interner: &mut StringInterner,
    ) -> Option<Value> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a + b)),
            (Value::Object(a), Value::Object(b)) => match (a.deref(), b.deref()) {
                (Object::String(a), Object::String(b)) => {
                    let res = format!("{}{}", a, b);
                    Some(Value::Object(heap.alloc(Object::String(interner.intern(&res)))))
                }
                _ => None,
            },
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

    pub fn as_instance(&self) -> Option<&Instance> {
        match self {
            Value::Object(o) => match o.deref() {
                Object::Instance(i) => Some(i),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_class(&self) -> Option<&Class> {
        match self {
            Value::Object(o) => match o.deref() {
                Object::Class(c) => Some(c),
                _ => None,
            },
            _ => None,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "Number({})", n),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Nil => write!(f, "Nil"),
            Value::Object(o) => write!(f, "{:?}", o),
        }
    }
}

use std::{fmt::Display, ops::{Add, Deref}, rc::Rc};

use crate::chunk::{StringInterner, Chunk};

use super::Value;

#[derive(Debug, Clone, PartialEq, derive_more::Display)]
pub enum Object {
    String(RloxString),
    Function(Function),
    NativeFun(NativeFun),
}

impl Object {
    pub fn add(self: Rc<Self>, other: Rc<Self>, interner: &impl StringInterner) -> Result<Value, (Value, Value)> {
        match (&*self, &*other) {
            (Object::String(a), Object::String(b) ) => {
                Ok(Value::string(&format!(
                    "{}{}",
                    a, b
                ), interner))
            }
            _ => Err((Value::Object(self), Value::Object(other))),
        }
    }
}

mod rlox_string;
pub use rlox_string::RloxString;

mod function;
pub use function::*;

impl Object {
    pub fn try_as_string(&self) -> Option<&RloxString> {
        match self {
            Object::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn try_as_function(&self) -> Option<&Function> {
        match self {
            Object::Function(f) => Some(f),
            _ => None,
        }
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        log::trace!("Dropping object: {:?}", self);
    }
}

use std::{fmt::Display, ops::{Add, Deref}, rc::Rc};

use crate::chunk::{StringInterner, Chunk};

use super::Value;

#[derive(Debug, Clone)]
pub struct Object {
    pub data: ObjectData,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Deref for Object {
    type Target = ObjectData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl Object {
    pub fn new(data: ObjectData) -> Self {
        let obj = Object { data };
        log::trace!("Allocated new object: {:?}", obj);
        obj
    }

    pub fn intern_strings<Interner: StringInterner>(&mut self, interner: &mut Interner) {
        match &mut self.data {
            ObjectData::String(s) => {
                interner.intern_string(s);
            }
            ObjectData::Function(f) => {
                f.intern_strings(interner);
            }
            ObjectData::NativeFun(_) => {}
        }
    }

    pub fn string(s: impl Into<RloxString>) -> Self {
        Self::new(ObjectData::String(s.into()))
    }
}

impl Add for Box<Object> {
    type Output = Result<Value, (Value, Value)>;
    fn add(self, other: Self) -> Self::Output {
        match (&*self, &*other) {
            (Object { data: ObjectData::String(a) }, Object { data: ObjectData::String(b) }) => {
                Ok(Value::Object(Box::new(Object::string(format!(
                    "{}{}",
                    a, b
                )))))
            }
            _ => Err((Value::Object(self), Value::Object(other))),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

mod rlox_string;
pub use rlox_string::RloxString;

mod function;
pub use function::*;

#[derive(Debug, Clone, PartialEq, derive_more::Display)]
pub enum ObjectData {
    String(RloxString),
    Function(Function),
    NativeFun(NativeFun),
}

impl ObjectData {
    pub fn try_as_string(&self) -> Option<&RloxString> {
        match self {
            ObjectData::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn unwrap_function(&self) -> Option<&Function> {
        match self {
            ObjectData::Function(f) => Some(f),
            _ => None,
        }
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        log::trace!("Dropping object: {:?}", self);
    }
}

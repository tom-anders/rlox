use std::{fmt::Display, ops::Add};

use super::Value;

#[derive(Debug, Clone)]
pub struct Object {
    data: ObjectData,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Object {
    pub fn new(data: ObjectData) -> Self {
        let obj = Object { data };
        log::trace!("Allocated new object: {:?}", obj);
        obj
    }
}

impl Add for Box<Object> {
    type Output = Result<Value, (Value, Value)>;
    fn add(self, other: Self) -> Self::Output {
        match (&*self, &*other) {
            (Object {
                data: ObjectData::String(a),
            }, Object {
                data: ObjectData::String(b),
            }) => Ok(Value::Object(Box::new(Object::new(ObjectData::String(format!("{}{}", a, b)))))),
            _ => Err((Value::Object(self), Value::Object(other))),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectData {
    String(String),
}

impl std::fmt::Display for ObjectData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectData::String(s) => write!(f, "{}", s),
        }
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        log::trace!("Dropping object: {:?}", self);
    }
}

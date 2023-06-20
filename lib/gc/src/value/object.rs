use std::{
    fmt::Display,
    ops::{Add, Deref},
    rc::Rc,
};

use crate::chunk::{Chunk, StringInterner};

use super::Value;

#[derive(Debug, Clone, PartialEq, derive_more::Display)]
pub enum Object {}

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

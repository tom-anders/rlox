use std::{fmt::{Debug, Display}, rc::Rc};

use crate::{
    chunk::{Chunk, StringInterner},
    value::Value,
};

use super::RloxString;

#[derive(Clone, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: RloxString,
}

impl Function {
    pub fn new(arity: usize, name: &str, interner: &impl StringInterner) -> Function {
        Function { arity, chunk: Chunk::default(), name: RloxString::new(name, interner) }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.name.as_str() {
            "" => write!(f, "<script>"),
            name => write!(f, "<fn {}>", name),
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let alternate = f.alternate();
        f.debug_struct("Function")
            .field("arity", &self.arity)
            // Printing out the chunk makes output to verbose,
            // do don't to it by default
            .field("chunk", if alternate { &self.chunk } else { &"<chunk>" })
            .field("name", &self.name)
            .finish()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NativeFun (pub fn(Vec<Value>) -> Value);

impl Display for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

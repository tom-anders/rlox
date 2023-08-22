use std::fmt::{Debug, Display};

use instructions::Arity;
use strings::string_interner::{InternedString, StringInterner};

use crate::{chunk::Chunk, value::Value};

#[derive(Clone, PartialEq)]
pub struct Function {
    pub arity: Arity,
    pub chunk: Chunk,
    pub name: InternedString,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.name {
            "" => write!(f, "<script>"),
            name => write!(f, "<fn {}>", name),
        }
    }
}

impl Function {
    pub fn new(arity: Arity, name: &str, interner: &mut StringInterner) -> Function {
        Function { arity, chunk: Chunk::default(), name: interner.intern(name) }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Intenionally ignoring the chunk here, since we'd need a heap ref to resolve it,
        // but it would make the output too verbose anyway.
        f.debug_struct("Function").field("arity", &self.arity).field("name", &self.name).finish()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NativeFun(pub fn(Vec<Value>) -> Value);

impl Display for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

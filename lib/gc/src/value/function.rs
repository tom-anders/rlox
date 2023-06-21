use std::fmt::{Debug, Display};

use instructions::Arity;

use crate::{chunk::Chunk, string_interner::StringInterner, value::Value};

use super::RloxString;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub arity: Arity,
    pub chunk: Chunk,
    pub name: RloxString,
}

pub struct FunctionDisplay<'a, 'b>(pub &'a Function, pub &'b StringInterner);

impl<'a, 'b> Display for FunctionDisplay<'a, 'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.name.resolve(self.1) {
            "" => write!(f, "<script>"),
            name => write!(f, "<fn {}>", name),
        }
    }
}

impl Function {
    pub fn new(arity: Arity, name: &str, interner: &mut StringInterner) -> Function {
        Function { arity, chunk: Chunk::default(), name: RloxString::new(name, interner) }
    }

    pub fn debug<'a, 'b>(&'a self, interner: &'b StringInterner) -> FunctionDebug<'a, 'b> {
        FunctionDebug(self, interner)
    }
}

pub struct FunctionDebug<'a, 'b>(pub &'a Function, pub &'b StringInterner);

impl Debug for FunctionDebug<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Intenionally ignoring the chunk here, since we'd need a heap ref to resolve it,
        // but it would make the output too verbose anyway.
        f.debug_struct("Function")
            .field("arity", &self.0.arity)
            .field("name", &self.0.name.resolve(self.1))
            .finish()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NativeFun(pub fn(Vec<Value>) -> Value);

impl Display for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

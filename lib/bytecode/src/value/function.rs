use std::fmt::{Debug, Display};

use crate::{
    chunk::Chunk,
    value::Value, string_interner::StringInterner,
};

use super::RloxString;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: RloxString,
}

pub struct FunctionDisplay<'a, 'b>(pub &'a Function, pub &'b StringInterner);

impl<'a, 'b> Display for FunctionDisplay<'a, 'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.name.as_str(self.1) {
            "" => write!(f, "<script>"),
            name => write!(f, "<fn {}>", name),
        }
    }
}

impl Function {
    pub fn new(arity: usize, name: &str, interner: &mut StringInterner) -> Function {
        Function { arity, chunk: Chunk::default(), name: RloxString::new(name, interner) }
    }

    pub fn debug<'a, 'b>(&'a self, interner: &'b StringInterner) -> FunctionDebug<'a, 'b> {
        FunctionDebug(self, interner)
    }
}

pub struct FunctionDebug<'a, 'b>(pub &'a Function, pub &'b StringInterner);

impl Debug for FunctionDebug<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let alternate = f.alternate();
        let chunk_debug = self.0.chunk.with_interner(self.1);
        f.debug_struct("Function")
            .field("arity", &self.0.arity)
            // Printing out the chunk makes output to verbose,
            // do don't to it by default
            .field("chunk", if alternate { &chunk_debug } else { &"<chunk>" })
            .field("name", &self.0.name.as_str(self.1))
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

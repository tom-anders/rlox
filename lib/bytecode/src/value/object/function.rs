use std::fmt::Display;

use crate::chunk::{Chunk, StringInterner};

use super::RloxString;

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: RloxString,
}

impl Function {
    pub fn intern_strings<Interner: StringInterner>(&mut self, interner: &mut Interner) {
        interner.intern_string(&mut self.name);
        self.chunk.intern_strings(interner);
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

use instructions::Arity;

use crate::FunctionRef;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    function: FunctionRef,
}

impl Closure {
    pub fn new(function: FunctionRef) -> Self {
        Self { function }
    }

    pub fn arity(&self) -> Arity {
        self.function.arity
    }

    pub fn function(&self) -> FunctionRef {
        self.function.clone()
    }

    pub fn chunk(&self) -> &crate::Chunk {
        &self.function.chunk
    }
}

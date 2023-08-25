use instructions::Arity;

use crate::{FunctionRef, UpvalueRef};

#[derive(Debug, Clone, PartialEq, derive_more::Display)]
#[display(fmt = "{function}")]
pub struct Closure {
    function: FunctionRef,
    upvalues: Vec<UpvalueRef>,
}

impl Closure {
    pub fn new(function: FunctionRef, upvalues: Vec<UpvalueRef>) -> Self {
        Self { function, upvalues }
    }

    pub fn upvalues(&self) -> &[UpvalueRef] {
        &self.upvalues
    }

    pub fn upvalues_mut(&mut self) -> &mut [UpvalueRef] {
        &mut self.upvalues
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

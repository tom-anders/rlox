use crate::{ClosureRef, InstanceRef};

#[derive(Clone, Debug, derive_more::Display)]
#[display(fmt = "{method}")]
pub struct BoundMethod {
    receiver: InstanceRef,
    method: ClosureRef,
}

// See operator/equals_method.lox in the official test suite,
// lox considers bound method to be equal if and only if they are the same object.
impl PartialEq for BoundMethod {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl BoundMethod {
    pub fn new(receiver: InstanceRef, method: ClosureRef) -> Self {
        Self { receiver, method }
    }

    pub(crate) fn method_mut(&mut self) -> &mut ClosureRef {
        &mut self.method
    }

    pub(crate) fn receiver_mut(&mut self) -> &mut InstanceRef {
        &mut self.receiver
    }

    pub fn method(&self) -> &ClosureRef {
        &self.method
    }

    pub fn receiver(&self) -> &InstanceRef {
        &self.receiver
    }
}

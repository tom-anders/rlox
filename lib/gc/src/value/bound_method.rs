use crate::{ClosureRef, InstanceRef};

#[derive(Clone, Debug, PartialEq)]
pub struct BoundMethod {
    receiver: InstanceRef,
    method: ClosureRef,
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

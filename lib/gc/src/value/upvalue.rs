use crate::ValueRef;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Upvalue {
    location: ValueRef,
}

impl Upvalue {
    pub fn new(location: ValueRef) -> Self {
        Self { location }
    }

    pub fn location(&self) -> ValueRef {
        self.location
    }

    pub fn location_mut(&mut self) -> &mut ValueRef {
        &mut self.location
    }
}

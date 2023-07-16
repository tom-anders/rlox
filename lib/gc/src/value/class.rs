use crate::RloxString;

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    name: RloxString,
}

impl Class {
    pub fn new(name: RloxString) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &RloxString {
        &self.name
    }
}

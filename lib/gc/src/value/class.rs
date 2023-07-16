use std::collections::HashMap;

use crate::{RloxString, ClosureRef};

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    name: RloxString,
    methods: HashMap<RloxString, ClosureRef>,
}

impl Class {
    pub fn new(name: RloxString) -> Self {
        Self { name, methods: HashMap::new() }
    }

    pub fn add_method(&mut self, name: RloxString, method: ClosureRef) {
        self.methods.insert(name, method);
    }

    pub fn get_method(&self, name: &RloxString) -> Option<&ClosureRef> {
        self.methods.get(name)
    }

    pub(crate) fn methods_mut(&mut self) -> &mut HashMap<RloxString, ClosureRef> {
        &mut self.methods
    }

    pub fn name(&self) -> &RloxString {
        &self.name
    }
}

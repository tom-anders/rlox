use strings::{string_interner::InternedString, table::StringTable};

use crate::ClosureRef;

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    name: InternedString,
    methods: StringTable<ClosureRef>,
}

impl Class {
    pub fn new(name: InternedString) -> Self {
        Self { name, methods: StringTable::new() }
    }

    pub fn add_method(&mut self, name: InternedString, method: ClosureRef) {
        self.methods.insert(name, method);
    }

    pub fn get_method(&self, name: &InternedString) -> Option<&ClosureRef> {
        self.methods.get(*name)
    }

    pub(crate) fn methods_mut(&mut self) -> &mut StringTable<ClosureRef> {
        &mut self.methods
    }

    pub fn name(&self) -> &InternedString {
        &self.name
    }
}

use strings::{string_interner::InternedString, table::StringTable};

use crate::{ClassRef, Value};

#[derive(Debug, Clone, PartialEq, derive_more::Display)]
#[display(fmt = "{} instance", class)]
pub struct Instance {
    class: ClassRef,
    fields: StringTable<Value>,
}

impl Instance {
    pub fn new(class: ClassRef, fields: StringTable<Value>) -> Self {
        Self { class, fields }
    }

    pub fn class(&self) -> &ClassRef {
        &self.class
    }

    pub(crate) fn class_mut(&mut self) -> &mut ClassRef {
        &mut self.class
    }

    pub fn field(&self, name: &InternedString) -> Option<&Value> {
        self.fields.get(*name)
    }

    pub fn set_field(&mut self, name: &InternedString, value: Value) {
        self.fields.insert(*name, value);
    }

    pub(crate) fn fields_mut(&mut self) -> &mut StringTable<Value> {
        &mut self.fields
    }
}

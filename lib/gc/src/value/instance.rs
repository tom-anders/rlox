use std::collections::HashMap;

use crate::{ClassRef, RloxString, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    class: ClassRef,
    fields: HashMap<RloxString, Value>,
}

impl Instance {
    pub fn new(class: ClassRef, fields: HashMap<RloxString, Value>) -> Self {
        Self { class, fields }
    }

    pub(crate) fn class(&self) -> &ClassRef {
        &self.class
    }

    pub(crate) fn class_mut(&mut self) -> &mut ClassRef {
        &mut self.class
    }

    pub fn field(&self, name: &RloxString) -> Option<&Value> {
        self.fields.get(name)
    }

    pub(crate) fn fields(&self) -> &HashMap<RloxString, Value> {
        &self.fields
    }

    pub(crate) fn fields_mut(&mut self) -> &mut HashMap<RloxString, Value> {
        &mut self.fields
    }
}

use std::{collections::{HashMap, hash_map::Entry}, ops::Deref};

use crate::{ClassRef, Value, Object, InternedString};

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    class: ClassRef,
    fields: HashMap<InternedString, Value>,
}

impl Instance {
    pub fn new(class: ClassRef, fields: HashMap<InternedString, Value>) -> Self {
        Self { class, fields }
    }

    pub fn class(&self) -> &ClassRef {
        &self.class
    }

    pub(crate) fn class_mut(&mut self) -> &mut ClassRef {
        &mut self.class
    }

    pub fn field(&self, name: &InternedString) -> Option<&Value> {
        self.fields.get(name)
    }

    pub fn field_mut(&mut self, name: &InternedString) -> Entry<InternedString, Value>  {
        self.fields.entry(*name)
    }

    pub(crate) fn fields(&self) -> &HashMap<InternedString, Value> {
        &self.fields
    }

    pub(crate) fn fields_mut(&mut self) -> &mut HashMap<InternedString, Value> {
        &mut self.fields
    }
}

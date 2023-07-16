use std::{collections::{HashMap, hash_map::Entry}, ops::Deref};

use crate::{ClassRef, RloxString, Value, Object};

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

    pub fn field_mut(&mut self, name: &RloxString) -> Entry<RloxString, Value>  {
        self.fields.entry(*name)
    }

    pub(crate) fn fields(&self) -> &HashMap<RloxString, Value> {
        &self.fields
    }

    pub(crate) fn fields_mut(&mut self) -> &mut HashMap<RloxString, Value> {
        &mut self.fields
    }
}

impl<'a> TryFrom<&'a Value> for &'a Instance {
    type Error = ();

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        if let Value::Object(object) = value {
            if let Object::Instance(instance) = object.deref() {
                return Ok(instance);
            }
        }

        Err(())
    }
}

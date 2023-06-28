mod object_ref;
use std::{pin::Pin, ops::DerefMut};

pub use object_ref::*;

use crate::{garbage_collector::GcObject, Object};

#[derive(Debug)]
pub struct Heap {
    pub(crate) objects: Vec<Pin<Box<GcObject>>>,
}

impl Heap {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            objects: Vec::with_capacity(cap),
        }
    }

    pub fn alloc(&mut self, object: impl Into<Object>) -> ObjectRef {
        self.objects.push(Box::pin(GcObject::new(object.into())));

        self.objects.last_mut().unwrap().deref_mut().into()
    }
}

use std::{marker::PhantomData, ops::{Deref, DerefMut}, pin::Pin};

use crate::{RloxString, StringInterner, Object, Function};

#[derive(Debug)]
pub struct Heap {
    objects: Vec<Pin<Box<Object>>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ObjectRef(*mut Object);

impl Deref for ObjectRef {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        // SAFETY: 
        // # Lifetime / validity
        // The only way to obtain a ObjectRef is through Heap::alloc.
        // Since values are stored as a Pin<Box<Object>>, the pointer will remain valid
        // for the lifetime of the heap, even if the vector reallocates.
        //
        // While removing an object from the heap would lead to a dangling pointer,
        // if the garbage collector is implemented correctly, it will never remove an 
        // object that is still reachable.
        //
        // # Aliasing
        // Right now, ValueRef contains a const pointer and only implements Deref, 
        // but not DerefMut. The heap does not provide any way to mutate values after allocation
        // either. This means currently only immutable references can ever coexist, but no mutable
        // ones. 
        unsafe { &*self.0 }
    }
}

impl ObjectRef {
    /// # Safety
    /// Callers must make sure this does not violate Rusts's aliasing rules,
    /// i.e. that there currently are no other mutable/immutable borrows of the same object.
    pub unsafe fn deref_mut(&mut self) -> &mut Object {
        &mut *self.0
    }

    pub fn unwrap_function(self) -> FunctionRef {
        self.try_into().unwrap()
    }

    pub fn unwrap_string(self) -> StringRef {
        self.try_into().unwrap()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypedObjectRef<T>(ObjectRef, PhantomData<T>);

pub type FunctionRef = TypedObjectRef<Function>;

impl<T> Deref for TypedObjectRef<T>
where
    for<'a> &'a T: TryFrom<&'a Object>,
    for<'a> <&'a T as std::convert::TryFrom<&'a Object>>::Error: std::fmt::Debug,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref().try_into().unwrap()
    }
}

impl <T> From<ObjectRef> for TypedObjectRef<T> {
    fn from(object: ObjectRef) -> Self {
        Self(object, PhantomData)
    }
}

impl<T> TypedObjectRef<T>
where 
    for<'a> &'a mut T: TryFrom<&'a mut Object>,
    for<'a> <&'a mut T as std::convert::TryFrom<&'a mut Object>>::Error: std::fmt::Debug,
{
    /// # Safety
    /// Same as ValueRef::deref_mut()
    pub unsafe fn deref_mut(&mut self) -> &mut T {
        self.0.deref_mut().try_into().unwrap()
    }
}

pub type StringRef = TypedObjectRef<RloxString>;

impl StringRef {
    pub fn resolve<'a>(&self, interner: &'a StringInterner) -> &'a str {
        self.deref().resolve(interner)
    }
}

impl Heap {
    pub fn with_capacity(cap: usize) -> Self {
        Self { objects: Vec::with_capacity(cap) }
    }

    pub fn alloc(&mut self, object: Object) -> ObjectRef {
        self.objects.push(Box::pin(object));

        ObjectRef(self.objects.last_mut().unwrap().deref_mut())
    }
}

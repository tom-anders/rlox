use std::{
    marker::PhantomData,
    ops::Deref,
};

#[cfg(feature = "refcount_objects")]
use std::rc::{Rc, Weak};

use strings::string_interner::InternedString;

use crate::{
    garbage_collector::GcObject, BoundMethod, Class, Closure, Function, Instance, Object, Upvalue,
    Value,
};

#[derive(Clone)]
pub struct ObjectRef(*mut GcObject, #[cfg(feature = "refcount_objects")] Weak<()>);

impl From<&mut GcObject> for ObjectRef {
    fn from(object: &mut GcObject) -> Self {
        Self(
            object,
            #[cfg(feature = "refcount_objects")]
            Rc::downgrade(&object._debug),
        )
    }
}

impl PartialEq for ObjectRef {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Deref for ObjectRef {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        self.debug_assert_not_dangling();
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
        // deref_mut() is marked unsafe, callers must make sure that Heap values only modified
        // when no other mutable/immutable references exist.
        // As long as this variant is upheld by deref_mut() callers, deref() is safe.
        unsafe { &(*self.0).object }
    }
}

impl std::fmt::Display for ObjectRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.deref())
    }
}

impl std::fmt::Debug for ObjectRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl ObjectRef {
    fn debug_assert_not_dangling(&self) {
        #[cfg(feature = "refcount_objects")]
        assert!(self.1.upgrade().is_some(), "Dangling pointer!");
    }

    /// # Safety
    /// Callers must make sure this does not violate Rusts's aliasing rules,
    /// i.e. that there currently are no other mutable/immutable borrows of the same object.
    pub unsafe fn deref_mut(&mut self) -> &mut Object {
        self.debug_assert_not_dangling();
        &mut (*self.0).object
    }

    pub fn is_marked(&self) -> bool {
        self.debug_assert_not_dangling();
        unsafe { (*self.0).is_marked() }
    }

    pub fn mark_reachable(&mut self) {
        self.debug_assert_not_dangling();
        unsafe {
            (*self.0).mark_reachable();
        }
    }

    pub fn unmark(&mut self) {
        self.debug_assert_not_dangling();
        unsafe {
            (*self.0).unmark();
        }
    }

    pub fn as_instance(self) -> Option<InstanceRef> {
        match self.deref() {
            Object::Instance(_) => Some(InstanceRef::from(self)),
            _ => None,
        }
    }

    pub fn unwrap_instance(self) -> InstanceRef {
        self.into()
    }

    pub fn unwrap_function(self) -> FunctionRef {
        self.into()
    }

    pub fn unwrap_closure(self) -> ClosureRef {
        self.into()
    }

    pub fn unwrap_class(self) -> ClassRef {
        self.into()
    }

    pub fn unwrap_string(self) -> StringRef {
        self.into()
    }
}

#[derive(Clone, PartialEq)]
pub struct TypedObjectRef<T>(ObjectRef, PhantomData<T>);

impl<T> std::fmt::Debug for TypedObjectRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.deref().fmt(f)
    }
}

pub type FunctionRef = TypedObjectRef<Function>;
pub type InstanceRef = TypedObjectRef<Instance>;
pub type ClosureRef = TypedObjectRef<Closure>;
pub type UpvalueRef = TypedObjectRef<Upvalue>;
pub type ClassRef = TypedObjectRef<Class>;
pub type BoundMethodRef = TypedObjectRef<BoundMethod>;

impl<T> std::fmt::Display for TypedObjectRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.deref())
    }
}

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

impl<T> From<ObjectRef> for TypedObjectRef<T> {
    fn from(object: ObjectRef) -> Self {
        Self(object, PhantomData)
    }
}

impl<T> From<TypedObjectRef<T>> for ObjectRef {
    fn from(object: TypedObjectRef<T>) -> Self {
        object.0
    }
}

impl<'a, T> From<&'a mut TypedObjectRef<T>> for &'a mut ObjectRef {
    fn from(object: &'a mut TypedObjectRef<T>) -> Self {
        &mut object.0
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

    pub fn mark_reachable(&mut self) {
        self.0.mark_reachable();
    }

    pub fn is_marked(&self) -> bool {
        self.0.is_marked()
    }
}

pub type StringRef = TypedObjectRef<InternedString>;

impl<T> From<TypedObjectRef<T>> for Value {
    fn from(object: TypedObjectRef<T>) -> Self {
        Value::Object(object.into())
    }
}

impl<T: Clone> From<&TypedObjectRef<T>> for Value {
    fn from(object: &TypedObjectRef<T>) -> Self {
        Value::Object((*object).clone().into())
    }
}

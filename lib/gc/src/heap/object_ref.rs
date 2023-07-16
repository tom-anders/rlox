use std::{rc::{Weak, Rc}, ops::Deref, marker::PhantomData};

use crate::{garbage_collector::GcObject, Object, Function, Closure, Upvalue, StringInterner, RloxString, Class, Instance, BoundMethod};

#[derive(Clone, Debug)]
pub struct ObjectRef(*mut GcObject, #[cfg(debug_assertions)] Weak<()>);

impl From<&mut GcObject> for ObjectRef {
    fn from(object: &mut GcObject) -> Self {
        Self(
            object,
            #[cfg(debug_assertions)]
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

impl ObjectRef {
    fn debug_assert_not_dangling(&self) {
        #[cfg(debug_assertions)]
        debug_assert!(self.1.upgrade().is_some(), "Dangling pointer!");
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

    pub fn unwrap_instance(self) -> InstanceRef {
        self.try_into().unwrap()
    }

    pub fn unwrap_function(self) -> FunctionRef {
        self.try_into().unwrap()
    }

    pub fn unwrap_closure(self) -> ClosureRef {
        self.try_into().unwrap()
    }

    pub fn unwrap_class(self) -> ClassRef {
        self.try_into().unwrap()
    }

    pub fn unwrap_string(self) -> StringRef {
        self.try_into().unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedObjectRef<T>(ObjectRef, PhantomData<T>);

pub type FunctionRef = TypedObjectRef<Function>;
pub type InstanceRef = TypedObjectRef<Instance>;
pub type ClosureRef = TypedObjectRef<Closure>;
pub type UpvalueRef = TypedObjectRef<Upvalue>;
pub type ClassRef = TypedObjectRef<Class>;
pub type BoundMethodRef = TypedObjectRef<BoundMethod>;

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

pub type StringRef = TypedObjectRef<RloxString>;

impl StringRef {
    pub fn resolve<'a>(&self, interner: &'a StringInterner) -> &'a str {
        self.deref().resolve(interner)
    }
}



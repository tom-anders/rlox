use std::{
    cell::RefCell,
    debug_assert,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    pin::Pin,
    rc::{Rc, Weak},
};

use itertools::Itertools;

use crate::{Closure, Function, GcObject, Object, RloxString, StringInterner, Upvalue, Value};

#[derive(Debug)]
pub struct Heap {
    objects: Vec<Pin<Box<GcObject>>>,
    gray_stack: Vec<ObjectRef>,
    marked_objects: Vec<ObjectRef>,
}

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

    pub fn unwrap_function(self) -> FunctionRef {
        self.try_into().unwrap()
    }

    pub fn unwrap_string(self) -> StringRef {
        self.try_into().unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedObjectRef<T>(ObjectRef, PhantomData<T>);

pub type FunctionRef = TypedObjectRef<Function>;
pub type ClosureRef = TypedObjectRef<Closure>;
pub type UpvalueRef = TypedObjectRef<Upvalue>;

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

impl Heap {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            objects: Vec::with_capacity(cap),
            gray_stack: Vec::new(),
            marked_objects: Vec::new(),
        }
    }

    pub fn gc_needed(&self) -> bool {
        if cfg!(feature = "gc_stress_test") {
            true
        } else {
            false
        }
    }

    pub fn collect_garbage(&mut self, interner: &StringInterner) {
        log::trace!("Starting garbage collection");

        log::trace!(
            "Gray stack: {:?}",
            self.gray_stack.iter().map(|o| o.resolve(interner)).collect_vec()
        );
        log::trace!(
            "Heap: {:?}",
            self.objects.iter().map(|o| o.object.resolve(interner)).collect_vec()
        );

        while let Some(mut object) = self.gray_stack.pop() {
            log::trace!("Marking {:?}", object.deref());
            unsafe {
                match object.deref_mut() {
                    Object::String(_) => {}
                    Object::NativeFun(..) => {}
                    Object::Upvalue(upvalue) => {
                        if let Upvalue::Closed(value) = upvalue {
                            self.mark_value(value);
                        }
                    }
                    Object::Function(fun) => {
                        for constant in fun.chunk.constants_mut() {
                            self.mark_value(constant);
                        }
                    }
                    Object::Closure(closure) => {
                        self.mark_ref(&mut closure.function());
                        for upvalue in closure.upvalues_mut() {
                            self.mark_ref(upvalue);
                        }
                    }
                }
            }
        }

        self.objects.retain_mut(|o| {
            let is_marked = o.is_marked();
            #[cfg(debug_assertions)]
            {
                let weak_count = Rc::weak_count(&o._debug);
                if !is_marked && weak_count != 0 {
                    log::warn!(
                        "Dropping an object that still has {weak_count} weak references! This either means there is a bug in the GC, or this is a cyclic reference"
                    );
                }
            }
            is_marked
        });

        for mut object in self.marked_objects.drain(..) {
            object.unmark();
        }

        self.gray_stack.clear();
    }

    pub fn mark_ref<'a>(&mut self, object_ref: impl Into<&'a mut ObjectRef>) {
        let object_ref = object_ref.into();
        if !object_ref.is_marked() {
            object_ref.mark_reachable();
            self.gray_stack.push(object_ref.clone());
            self.marked_objects.push(object_ref.clone());
        }
    }

    pub fn mark_value(&mut self, value: &mut Value) {
        if let Value::Object(object) = value {
            self.mark_ref(object);
        }
    }

    pub fn alloc(&mut self, object: impl Into<Object>) -> ObjectRef {
        self.objects.push(Box::pin(GcObject::new(object.into())));

        self.objects.last_mut().unwrap().deref_mut().into()
    }
}

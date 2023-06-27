use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    pin::Pin,
};

use itertools::Itertools;

use crate::{Closure, Function, GcObject, Object, RloxString, StringInterner, Upvalue, Value};

#[derive(Debug)]
pub struct Heap {
    // TODO For debug mode, can we let GcObject contain a Rc,
    // and each reference a Weak? Then we can use this to check for dangling pointers.
    objects: Vec<Pin<Box<GcObject>>>,
    gray_stack: Vec<ObjectRef>,
    marked_objects: Vec<ObjectRef>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ObjectRef(*mut GcObject);

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
        // deref_mut() is marked unsafe, callers must make sure that Heap values only modified
        // when no other mutable/immutable references exist.
        // As long as this variant is upheld by deref_mut() callers, deref() is safe.
        unsafe { &(*self.0).object }
    }
}

impl ObjectRef {
    /// # Safety
    /// Callers must make sure this does not violate Rusts's aliasing rules,
    /// i.e. that there currently are no other mutable/immutable borrows of the same object.
    pub unsafe fn deref_mut(&mut self) -> &mut Object {
        &mut (*self.0).object
    }

    pub fn is_marked(&self) -> bool {
        unsafe { (*self.0).is_marked() }
    }

    pub fn mark_reachable(&mut self) {
        unsafe {
            (*self.0).mark_reachable();
        }
    }

    pub fn unmark(&mut self) {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
                        self.mark_ref(closure.function());
                        for upvalue in closure.upvalues_mut() {
                            self.mark_ref(*upvalue);
                        }
                    }
                }
            }
        }

        self.objects.retain_mut(|o| o.is_marked());

        for mut object in self.marked_objects.drain(..) {
            object.unmark();
        }

        self.gray_stack.clear();
    }

    pub fn mark_ref(&mut self, object_ref: impl Into<ObjectRef>) {
        let mut object_ref = object_ref.into();
        if !object_ref.is_marked() {
            object_ref.mark_reachable();
            self.gray_stack.push(object_ref);
            self.marked_objects.push(object_ref);
        }
    }

    pub fn mark_value(&mut self, value: &mut Value) {
        if let Value::Object(object) = value {
            self.mark_ref(*object);
        }
    }

    pub fn alloc(&mut self, object: impl Into<Object>) -> ObjectRef {
        self.objects.push(Box::pin(GcObject::new(object.into())));

        ObjectRef(self.objects.last_mut().unwrap().deref_mut())
    }
}

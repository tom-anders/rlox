#[cfg(feature = "refcount_objects")]
use std::rc::Rc;

use crate::{Heap, Object, ObjectRef, Upvalue, Value};

#[derive(Debug, PartialEq)]
pub(crate) struct GcObject {
    is_marked: bool,
    pub(crate) object: Object,
    #[cfg(feature = "refcount_objects")]
    pub(crate) _debug: Rc<()>,
}

impl GcObject {
    pub(crate) fn new(object: impl Into<Object>) -> Self {
        return Self {
            is_marked: false,
            object: object.into(),
            #[cfg(feature = "refcount_objects")]
            _debug: Rc::new(()),
        };
    }

    pub(crate) fn mark_reachable(&mut self) {
        self.is_marked = true;
    }

    pub(crate) fn unmark(&mut self) {
        self.is_marked = false;
    }

    pub(crate) fn is_marked(&self) -> bool {
        match self.object {
            // clox also marks strings, we don't (yet?)
            Object::String(_) => true,
            _ => self.is_marked,
        }
    }
}

#[derive(Debug)]
pub struct GarbageCollector {
    gray_stack: Vec<ObjectRef>,
    marked_objects: Vec<ObjectRef>,
    next_gc: usize,
}

impl Default for GarbageCollector {
    fn default() -> Self {
        Self::new(1024 * 1024)
    }
}

impl GarbageCollector {
    pub fn new(next_gc: usize) -> Self {
        Self { gray_stack: Vec::new(), marked_objects: Vec::new(), next_gc }
    }

    pub fn gc_needed(&self, heap: &mut Heap) -> bool {
        if cfg!(feature = "gc_stress_test") {
            true
        } else {
            heap.objects.len() >= self.next_gc
        }
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

    pub fn collect_garbage(&mut self, heap: &mut Heap) {
        log::trace!("Starting garbage collection");

        log::trace!("Gray stack: {:?}", self.gray_stack);
        log::trace!("Heap: {:?}", heap.objects);

        while let Some(mut object) = self.gray_stack.pop() {
            log::trace!("Marking {object:?}");
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
                    Object::Class(class) => {
                        for method in class.methods_mut().values_mut() {
                            self.mark_ref(method);
                        }
                    }
                    Object::Instance(instance) => {
                        self.mark_ref(instance.class_mut());
                        for field in instance.fields_mut().values_mut() {
                            self.mark_value(field);
                        }
                    }
                    Object::BoundMethod(bound_method) => {
                        self.mark_ref(bound_method.receiver_mut());
                        self.mark_ref(bound_method.method_mut());
                    }
                }
            }
        }

        heap.objects.retain_mut(|o| {
            let is_marked = o.is_marked();
            #[cfg(feature = "refcount_objects")]
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

        self.next_gc = heap.objects.len() * 2;
    }
}

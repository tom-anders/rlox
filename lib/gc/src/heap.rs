use std::{marker::PhantomData, ops::{Deref, DerefMut}, pin::Pin};

use crate::{Function, RloxString, StringInterner, Value, Closure, Upvalue};

#[derive(Debug)]
pub struct Heap {
    objects: Vec<Pin<Box<Value>>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ValueRef(*mut Value);

impl Deref for ValueRef {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        // SAFETY:
        // # Lifetime / validity
        // The only way to obtain a ValueRef is through Heap::alloc.
        // Since values are stored as a Pin<Box<Value>>, the pointer will remain valid
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
        unsafe { &*self.0 }
    }
}

impl ValueRef {
    /// # Safety
    /// Callers must make sure this does not violate Rusts's aliasing rules,
    /// i.e. that there currently are no other mutable/immutable borrows of the same value.
    pub unsafe fn deref_mut(&mut self) -> &mut Value {
        &mut *self.0
    }
}

impl ValueRef {
    pub fn is_truthy(&self) -> bool {
        !matches!(self.deref(), Value::Nil | Value::Boolean(false))
    }

    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    pub fn equals(&self, other: ValueRef) -> bool {
        self.deref() == other.deref()
    }

    pub fn less_than(&self, other: ValueRef) -> Option<Value> {
        match (self.deref(), other.deref()) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Boolean(a < b)),
            _ => None,
        }
    }

    pub fn greater_than(&self, other: ValueRef) -> Option<Value> {
        match (self.deref(), other.deref()) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Boolean(a > b)),
            _ => None,
        }
    }

    pub fn negate(&self) -> Option<Value> {
        match self.deref() {
            Value::Number(n) => Some(Value::Number(-n)),
            _ => None,
        }
    }

    pub fn add(&self, other: ValueRef, interner: &mut StringInterner) -> Option<Value> {
        match (self.deref(), other.deref()) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a + b)),
            (Value::String(a), Value::String(b)) => {
                let res = format!("{}{}", a.resolve(interner), b.resolve(interner));
                Some(Value::String(RloxString::new(&res, interner)))
            }
            _ => None,
        }
    }

    pub fn subtract(&self, other: ValueRef) -> Option<Value> {
        match (self.deref(), other.deref()) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a - b)),
            _ => None,
        }
    }

    pub fn multiply(&self, other: ValueRef) -> Option<Value> {
        match (self.deref(), other.deref()) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a * b)),
            _ => None,
        }
    }

    pub fn divide(&self, other: ValueRef) -> Option<Value> {
        match (self.deref(), other.deref()) {
            (Value::Number(a), Value::Number(b)) => Some(Value::Number(a / b)),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypedValueRef<T>(ValueRef, PhantomData<T>);

pub type FunctionRef = TypedValueRef<Function>;

pub type ClosureRef = TypedValueRef<Closure>;

pub type UpvalueRef = TypedValueRef<Upvalue>;

impl<T> Deref for TypedValueRef<T>
where
    for<'a> &'a T: TryFrom<&'a Value>,
    for<'a> <&'a T as std::convert::TryFrom<&'a Value>>::Error: std::fmt::Debug,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref().try_into().unwrap()
    }
}

impl <T> From<ValueRef> for TypedValueRef<T> {
    fn from(value: ValueRef) -> Self {
        Self(value, PhantomData)
    }
}

impl<T> TypedValueRef<T>
where 
    for<'a> &'a mut T: TryFrom<&'a mut Value>,
    for<'a> <&'a mut T as std::convert::TryFrom<&'a mut Value>>::Error: std::fmt::Debug,
{
    /// # Safety
    /// Same as ValueRef::deref_mut()
    pub unsafe fn deref_mut(&mut self) -> &mut T {
        self.0.deref_mut().try_into().unwrap()
    }
}

pub type StringRef = TypedValueRef<RloxString>;

impl StringRef {
    pub fn resolve<'a>(&self, interner: &'a StringInterner) -> &'a str {
        self.deref().resolve(interner)
    }
}

impl Heap {
    pub fn with_capacity(cap: usize) -> Self {
        Self { objects: Vec::with_capacity(cap) }
    }

    pub fn alloc(&mut self, value: Value) -> ValueRef {
        self.objects.push(Box::pin(value));

        ValueRef(self.objects.last_mut().unwrap().deref_mut())
    }
}

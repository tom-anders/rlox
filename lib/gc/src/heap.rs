use std::{marker::PhantomData, ops::Deref, pin::Pin};

use crate::{Function, RloxString, StringInterner, Value, Closure};

#[derive(Debug)]
pub struct Heap {
    objects: Vec<Pin<Box<Value>>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ValueRef(*const Value);

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
        // Right now, ValueRef contains a const pointer and only implements Deref,
        // but not DerefMut. The heap does not provide any way to mutate values after allocation
        // either. This means currently only immutable references can ever coexist, but no mutable
        // ones.
        // TODO: Revisit this once we have objects
        unsafe { &*self.0 }
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

        ValueRef(self.objects.last().unwrap().deref())
    }
}

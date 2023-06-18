use std::{
    fmt::{Display, Formatter},
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
};

mod object;
pub use object::*;

use crate::chunk::StringInterner;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Object(Rc<Object>),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    pub fn string(s: &str, interner: &impl StringInterner) -> Value {
        Value::Object(Rc::new(Object::String(RloxString::new(s, interner))))
    }

    pub fn function(f: Function) -> Value {
        Value::Object(Rc::new(Object::Function(f)))
    }

    pub fn native_function(f: NativeFun) -> Value {
        Value::Object(Rc::new(Object::NativeFun(f)))
    }

    pub fn try_as_string(&self) -> Option<&RloxString> {
        match &self {
            Value::Object(o) => o.try_as_string(),
            _ => None,
        }
    }

    pub fn less_than(self, other: Value) -> Result<Value, (Value, Value)> {
        match (&self, &other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
            _ => Err((self, other)),
        }
    }

    pub fn greater_than(self, other: Value) -> Result<Value, (Value, Value)> {
        match (&self, &other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
            _ => Err((self, other)),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Object(o) => write!(f, "{}", o),
        }
    }
}

impl Neg for Value {
    type Output = Result<Self, Self>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            _ => Err(self),
        }
    }
}

impl Value {
    pub fn add(self, rhs: Self, interner: &impl StringInterner) -> Result<Value, (Value, Value)> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::Object(a), Value::Object(b)) => a.add(b, interner),
            (a, b) => Err((a, b)),
        }
    }
}

impl Sub for Value {
    type Output = Result<Self, (Self, Self)>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            _ => Err((self, rhs)),
        }
    }
}

impl Mul for Value {
    type Output = Result<Self, (Self, Self)>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            _ => Err((self, rhs)),
        }
    }
}

impl Div for Value {
    type Output = Result<Self, (Self, Self)>;

    fn div(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
            _ => Err((self, rhs)),
        }
    }
}

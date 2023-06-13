use std::{fmt::{Display, Formatter}, ops::{Neg, Add, Mul, Sub, Div}};

mod object;
pub use object::*;

use crate::chunk::StringInterner;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    // FIXME: Box is probably not correct here, clox just uses a raw pointer of course.
    // We'll have to see how this works out when we add GC.
    Object(Box<Object>)
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    pub fn intern_string<Interner: StringInterner>(&mut self, interner: &mut Interner) {
        if let Value::Object(o) = self {
            o.intern_string(interner)
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

impl Add for Value {
    type Output = Result<Self, (Self, Self)>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::Object(a), Value::Object(b)) => a + b,
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

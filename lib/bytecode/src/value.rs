use std::{
    ops::{Div, Mul, Neg, Sub},
    rc::Rc,
};

mod rlox_string;
pub use rlox_string::RloxString;

mod function;
pub use function::*;

use crate::chunk::StringInterner;

#[derive(Debug, Clone, PartialEq, derive_more::From, derive_more::TryInto, derive_more::Display)]
#[try_into(owned, ref, ref_mut)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    #[display(fmt = "nil")]
    Nil,
    String(RloxString),
    Function(Rc<Function>),
    NativeFun(NativeFun),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
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
            (Value::String(a), Value::String(b)) => {
                Ok(Value::String(RloxString::new(&format!("{}{}", a, b), interner)))
            }
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

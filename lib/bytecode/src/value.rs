use std::{
    ops::{Div, Mul, Neg, Sub},
    rc::Rc,
};

mod rlox_string;
pub use rlox_string::RloxString;

mod function;
pub use function::*;

use crate::string_interner::StringInterner;

#[derive(Clone, Debug, PartialEq, derive_more::From, derive_more::TryInto)]
#[try_into(owned, ref, ref_mut)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    String(RloxString),
    Function(Rc<Function>),
    NativeFun(NativeFun),
}

impl Value {
    pub fn with_interner<'a, 'b>(&'a self, interner: &'b StringInterner) -> ValueWithInterner<'a, 'b> {
        ValueWithInterner(self, interner)
    }
}

#[derive(Clone)]
pub struct ValueWithInterner<'a, 'b>(&'a Value, &'b StringInterner);

impl std::fmt::Debug for ValueWithInterner<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueWithInterner(Value::Number(n), _) => write!(f, "Number({})", n),
            ValueWithInterner(Value::Boolean(b), _) => write!(f, "Boolean({})", b),
            ValueWithInterner(Value::Nil, _) => write!(f, "Nil"),
            ValueWithInterner(Value::String(s), interner) => write!(f, "String({})", s.as_str(interner)),
            ValueWithInterner(Value::Function(fun), interner) => {
                write!(f, "Function({:?})", FunctionDebug(fun, interner))
            }
            ValueWithInterner(Value::NativeFun(fun), _) => write!(f, "NativeFun({})", fun),
        }
    }
}

impl std::fmt::Display for ValueWithInterner<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueWithInterner(Value::Number(n), _) => write!(f, "{}", n),
            ValueWithInterner(Value::Boolean(b), _) => write!(f, "{}", b),
            ValueWithInterner(Value::Nil, _) => write!(f, "nil"),
            ValueWithInterner(Value::String(s), interner) => write!(f, "{}", s.as_str(interner)),
            ValueWithInterner(Value::Function(fun), interner) => {
                write!(f, "{}", FunctionDisplay(fun, interner))
            }
            ValueWithInterner(Value::NativeFun(fun), _) => write!(f, "{}", fun),
        }
    }
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
    pub fn add(self, rhs: Self, interner: &mut StringInterner) -> Result<Value, (Value, Value)> {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::String(a), Value::String(b)) => {
                let res = format!("{}{}", a.as_str(interner), b.as_str(interner));
                Ok(Value::String(RloxString::new(&res, interner)))
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

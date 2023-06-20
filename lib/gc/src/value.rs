use std::ops::{Div, Mul, Neg, Sub};

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
    Function(Function),
    NativeFun(NativeFun),
}

impl Value {
    pub fn resolve<'a, 'b>(&'a self, interner: &'b StringInterner) -> ValueWithInterner<'a, 'b> {
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
            ValueWithInterner(Value::String(s), interner) => write!(f, "String({})", s.resolve(interner)),
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
            ValueWithInterner(Value::String(s), interner) => write!(f, "{}", s.resolve(interner)),
            ValueWithInterner(Value::Function(fun), interner) => {
                write!(f, "{}", FunctionDisplay(fun, interner))
            }
            ValueWithInterner(Value::NativeFun(fun), _) => write!(f, "{}", fun),
        }
    }
}

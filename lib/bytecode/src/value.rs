use std::{fmt::{Display, Formatter}, ops::{Neg, Add, Mul, Sub, Div}};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(false) => false,
            _ => true,
        }
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

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
        }
    }
}

impl Neg for Value {
    type Output = Result<Self, Self>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            Value::Boolean(_) => Err(self),
            Value::Nil => Err(self),
        }
    }
}

impl Add for Value {
    type Output = Result<Self, (Self, Self)>;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            // TODO add Value::String here once we have it
            _ => Err((self, rhs)),
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

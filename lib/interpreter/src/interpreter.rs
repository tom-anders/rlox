use std::unreachable;

use parser::{Expr, LiteralValue, Stmt};

mod value;
use scanner::{token::TokenData, Token};
use value::*;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Tried to apply unary minus on non-number value: {0}")]
    UnaryMinusOnNonNumber(Value),
    #[error("Tried to apply binary op on at least one non-number value: {0} {1}")]
    BinaryOpOnUnsupportedValue(Value, Value),
    #[error("Tried to divide by zero")]
    DivisionByZero,
}

#[derive(Debug)]
pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, stmts: &[Stmt]) -> Result<(), Error> {
        for s in stmts {
            self.execute(s)?;
        }
        Ok(())
    }

    fn execute(&self, stmt: &Stmt) -> Result<(), Error> {
        use Stmt::*;
        match stmt {
            Print(expr) => {
                let value = self.evaluate(expr)?;
                println!("{}", value);
                Ok(())
            }
            Expression(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Var{name, initializer} => {
                todo!()
            }
        }
    }

    fn evaluate(&self, expr: &Expr) -> Result<Value, Error> {
        use Expr::*;
        match expr {
            Literal(LiteralValue::Number(n)) => Ok((*n).into()),
            Literal(LiteralValue::Str(s)) => Ok((*s).into()),
            Literal(LiteralValue::Boolean(b)) => Ok((*b).into()),
            Literal(LiteralValue::Nil) => Ok(Value::Nil),

            Grouping(expr) => self.evaluate(expr),

            Unary { operator, right } => {
                let right = self.evaluate(right)?;
                match (&operator.data, right) {
                    (TokenData::Minus, Value::Number(n)) => Ok((-n).into()),
                    (TokenData::Minus, v) => Err(Error::UnaryMinusOnNonNumber(v)),
                    (TokenData::Bang, v) => Ok((!v.is_truthy()).into()),
                    _ => unreachable!(),
                }
            }

            Variable(token) => {
                todo!()
            }

            Binary { left, operator, right } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                match (&left, &right, &operator.data) {
                    (Value::Number(l), Value::Number(r), TokenData::Minus) => Ok((l - r).into()),
                    (Value::Number(l), Value::Number(r), TokenData::Slash) => {
                        if *r != 0.0 {
                            Ok((l / r).into())
                        } else {
                            Err(Error::DivisionByZero)
                        }
                    }
                    (Value::Number(l), Value::Number(r), TokenData::Star) => Ok((l * r).into()),
                    (Value::Number(l), Value::Number(r), TokenData::Plus) => Ok((l + r).into()),
                    (Value::Str(l), Value::Str(r), TokenData::Plus) => Ok((l.clone() + r).into()),

                    (Value::Number(l), Value::Number(r), TokenData::Greater) => Ok((l > r).into()),
                    (Value::Number(l), Value::Number(r), TokenData::GreaterEqual) => {
                        Ok((l >= r).into())
                    }
                    (Value::Number(l), Value::Number(r), TokenData::Less) => Ok((l < r).into()),
                    (Value::Number(l), Value::Number(r), TokenData::LessEqual) => {
                        Ok((l <= r).into())
                    }

                    (_, _, TokenData::EqualEqual) => Ok(left.eq_in_lox(&right).into()),
                    (_, _, TokenData::BangEqual) => Ok((!left.eq_in_lox(&right)).into()),

                    (_, _, TokenData::Plus) => Err(Error::BinaryOpOnUnsupportedValue(left, right)),
                    (_, _, TokenData::Minus) => Err(Error::BinaryOpOnUnsupportedValue(left, right)),
                    (_, _, TokenData::Slash) => Err(Error::BinaryOpOnUnsupportedValue(left, right)),
                    (_, _, TokenData::Star) => Err(Error::BinaryOpOnUnsupportedValue(left, right)),

                    _ => unreachable!(),
                }
            }
        }
    }
}

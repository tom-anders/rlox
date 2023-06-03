use std::fmt::{self, Display, Formatter};

use scanner::Token;

#[derive(Debug)]
pub enum Expr<'a> {
    Binary { left: Box<Expr<'a>>, operator: Token<'a>, right: Box<Expr<'a>> },
    Grouping(Box<Expr<'a>>),
    Unary { operator: Token<'a>, right: Box<Expr<'a>> },
    Literal(LiteralValue<'a>),
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary { left, operator, right } => {
                write!(f, "({} {} {})", operator, left, right)
            }
            Expr::Grouping(expression) => {
                write!(f, "(group {})", expression)
            }
            Expr::Unary { operator, right } => {
                write!(f, "({} {})", operator, right)
            }
            Expr::Literal(value) => {
                write!(f, "{}", value)
            }
        }
    }
}

#[derive(Debug)]
pub enum LiteralValue<'a> {
    Number(f64),
    Str(&'a str),
    Boolean(bool),
    Nil,
}

impl<'a> Display for LiteralValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LiteralValue::Number(n) => n.to_string(),
                LiteralValue::Str(s) => s.to_string(),
                LiteralValue::Boolean(b) => b.to_string(),
                LiteralValue::Nil => "nil".to_string(),
            }
        )
    }
}

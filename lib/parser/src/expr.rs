use std::fmt::{self, Display, Formatter};

use scanner::Token;

#[derive(Debug)]
pub enum Expr<'a> {
    Binary { left: Box<Expr<'a>>, operator: Token<'a>, right: Box<Expr<'a>> },
    Logical { left: Box<Expr<'a>>, operator: Token<'a>, right: Box<Expr<'a>> },
    Grouping(Box<Expr<'a>>),
    Unary { operator: Token<'a>, right: Box<Expr<'a>> },
    Literal(LiteralValue<'a>),
    Variable(Token<'a>),
    Assign { name: Token<'a>, value: Box<Expr<'a>> },
    Call { callee: Box<Expr<'a>>, closing_paren: Token<'a>, arguments: Vec<Expr<'a>> },
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary { left, operator, right } => {
                write!(f, "({} {} {})", operator, left, right)
            }
            Expr::Logical { left, operator, right } => {
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
            Expr::Variable(token) => {
                write!(f, "{}", token.lexeme())
            }
            Expr::Assign { name, value } => {
                write!(f, "(assign {} {})", name.lexeme(), value)
            }
            Expr::Call { callee, closing_paren, arguments } => {
                write!(
                    f,
                    "(call {} {} {})",
                    callee,
                    closing_paren,
                    arguments.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(" ")
                )
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

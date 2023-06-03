use crate::Token;

#[derive(Debug)]
pub enum Expr<'a> {
    Binary { left: Box<Expr<'a>>, operator: Token<'a>, right: Box<Expr<'a>> },
    Grouping { expression: Box<Expr<'a>> },
    Unary { operator: Token<'a>, right: Box<Expr<'a>> },
    Literal { value: LiteralValue<'a> },
}

#[derive(Debug)]
pub enum LiteralValue<'a> {
    Number(f64),
    Str(&'a str),
    Boolean(bool),
    Nil,
}

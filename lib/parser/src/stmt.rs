use scanner::Token;

use crate::Expr;

#[derive(Debug)]
pub enum Stmt<'a> {
    Expression(Box<Expr<'a>>),
    Print(Box<Expr<'a>>),
    Var{name: Token<'a>, initializer: Option<Box<Expr<'a>>>},
}


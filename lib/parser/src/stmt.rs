use scanner::Token;

use crate::Expr;

#[derive(Debug)]
pub enum Stmt<'a> {
    Expression(Box<Expr<'a>>),
    Print(Box<Expr<'a>>),
    Var{name: Token<'a>, initializer: Option<Box<Expr<'a>>>},
    Block(Vec<Stmt<'a>>),
    If{condition: Box<Expr<'a>>, then_branch: Box<Stmt<'a>>, else_branch: Option<Box<Stmt<'a>>>},
}


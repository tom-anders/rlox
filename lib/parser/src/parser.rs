mod expr;
mod stmt;
pub use stmt::Stmt;
use std::{mem::discriminant, cell::RefCell, println};

pub use expr::{Expr, LiteralValue};
use scanner::{token::TokenData, Token};

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    current: RefCell<usize>,
}

use TokenData::*;

#[derive(Debug, thiserror::Error)]
pub enum Error<'a> {
    #[error("error (l. {}): Missing closing `)` after `{}`", .0.line, .0.lexeme)]
    MissingRightParen(&'a Token<'a>),
    #[error("error (l. {}): Expected primary expression, found: `{}`", .0.line, .0.lexeme)]
    ExpectedPrimaryExpression(&'a Token<'a>),
    #[error("error (l. {}): Expected semicolon after {}", .0.line, .0.lexeme)]
    ExpectedSemicolon(&'a Token<'a>),
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self { tokens, current: 0.into() }
    }

    pub fn parse(&self) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut errors = Vec::new();
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(stmts)
        } else {
            Err(errors)
        }
    }

    fn declaration(&self) -> Result<Stmt, Error> {
        if self.consume(Var) {
            // self.var_declaration()
            todo!()
        } else {
            self.statement().map_err(|e| {
                self.synchronize();
                e
            })
        }
    }

    fn statement(&self) -> Result<Stmt, Error> {
        match self.peek() {
            Some(Print) => self.print_statement(),
            _ => self.expression_statement(),
        }
    }
    
    fn print_statement(&self) -> Result<Stmt, Error> {
        self.advance();
        let value = self.expression()?;
        if self.consume(Semicolon) {
            Ok(Stmt::Print ( value ))
        } else {
            Err(Error::ExpectedSemicolon(self.previous()))
        }
    }

    fn expression_statement(&self) -> Result<Stmt, Error> {
        let value = self.expression()?;
        if self.consume(Semicolon) {
            Ok(Stmt::Expression ( value ))
        } else {
            Err(Error::ExpectedSemicolon(self.previous()))
        }
    }

    fn expression(&self) -> Result<Box<Expr>, Error> {
        self.equality()
    }

    fn equality(&self) -> Result<Box<Expr>, Error> {
        let mut expr = self.comparison()?;

        while let Some(BangEqual) | Some(EqualEqual) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Box::new(Expr::Binary { left: expr, operator, right })
        }
        Ok(expr)
    }

    fn comparison(&self) -> Result<Box<Expr>, Error> {
        let mut expr = self.term()?;

        while let Some(Greater) | Some(GreaterEqual) | Some(Less) | Some(LessEqual) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Box::new(Expr::Binary { left: expr, operator, right })
        }
        Ok(expr)
    }

    fn term(&self) -> Result<Box<Expr>, Error> {
        let mut expr = self.factor()?;

        while let Some(Plus) | Some(Minus) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Box::new(Expr::Binary { left: expr, operator, right })
        }
        Ok(expr)
    }

    fn factor(&self) -> Result<Box<Expr>, Error> {
        let mut expr = self.unary()?;

        while let Some(Star) | Some(Slash) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Box::new(Expr::Binary { left: expr, operator, right })
        }
        Ok(expr)
    }

    fn unary(&self) -> Result<Box<Expr>, Error> {
        if let Some(Minus) | Some(Bang) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Box::new(Expr::Unary { operator, right }));
        }
        self.primary()
    }

    fn primary(&self) -> Result<Box<Expr>, Error> {
        match self.peek() {
            Some(False) => {
                self.advance();
                Ok(Box::new(Expr::Literal(LiteralValue::Boolean(false))))
            }
            Some(True) => {
                self.advance();
                Ok(Box::new(Expr::Literal(LiteralValue::Boolean(true))))
            }
            Some(Str(s)) => {
                self.advance();
                Ok(Box::new(Expr::Literal(LiteralValue::Str(s))))
            }
            Some(Number(n)) => {
                self.advance();
                Ok(Box::new(Expr::Literal(LiteralValue::Number(*n))))
            }
            Some(Nil) => {
                self.advance();
                Ok(Box::new(Expr::Literal(LiteralValue::Nil)))
            }
            Some(LeftParen) => {
                self.advance();
                let expr = self.expression()?;

                if self.consume(RightParen) {
                    Ok(Box::new(Expr::Grouping(expr)))
                } else {
                    Err(Error::MissingRightParen(self.previous()))
                }
            }
            _ => Err(Error::ExpectedPrimaryExpression(self.peek_token().unwrap())),
        }
    }

    fn consume(&self, token: TokenData) -> bool {
        assert!(!matches!(token, Number(_) | Str(_)));
        match self.peek() {
            Some(t) if t == &token => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn synchronize(&self) {
        loop {
            if self.is_at_end() {
                return;
            }

            match self.advance().data {
                Semicolon => return,
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => {}
            };
        }
    }

}

// Helpers
impl<'a> Parser<'a> {
    fn peek(&self) -> Option<&TokenData> {
        self.peek_token().map(|t| &t.data)
    }

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(*self.current.borrow())
    }

    fn advance(&self) -> &Token<'a> {
        assert!(!self.is_at_end());

        *self.current.borrow_mut() += 1;

        self.previous()
    }

    fn previous(&self) -> &Token<'a> {
        &self.tokens[*self.current.borrow() - 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek() == Some(&Eof)
    }
}

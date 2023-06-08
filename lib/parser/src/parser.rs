mod expr;
mod stmt;
use errors::{RloxError, RloxErrors};
use std::{cell::RefCell, println};
pub use stmt::Stmt;

pub use expr::{Expr, LiteralValue};
use scanner::{token::TokenData, Token};

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    current: RefCell<usize>,
}

use TokenData::*;

#[derive(Debug, thiserror::Error)]
pub struct ParserError<'a> {
    error: ParserErrorType,
    token: &'a Token<'a>,
}

impl<'a> From<ParserError<'a>> for RloxError {
    fn from(error: ParserError<'a>) -> Self {
        RloxError { line: error.token.line, col: error.token.col_start, message: error.to_string() }
    }
}

impl<'a> ParserError<'a> {
    fn new(error: ParserErrorType, token: &'a Token<'a>) -> Self {
        Self { token, error }
    }
}

impl std::fmt::Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.error {
                ParserErrorType::MissingRightParen =>
                    format!("Missing closing `)` after `{}`", self.token.lexeme),
                ParserErrorType::ExpectedPrimaryExpression =>
                    format!("Expected primary expression, found: `{}`", self.token.lexeme),
                ParserErrorType::ExpectedSemicolon =>
                    format!("Expected semicolon after {}", self.token.lexeme),
                ParserErrorType::ExpectedIdentifier =>
                    format!("Expected identifier after {}", self.token.lexeme),
                ParserErrorType::InvalidAssignmentTarget =>
                    format!("Invalid assignment target {}", self.token.lexeme),
            }
        )
    }
}

struct ParserErrors<'a>(Vec<ParserError<'a>>);

impl<'a> From<ParserErrors<'a>> for RloxErrors {
    fn from(errors: ParserErrors<'a>) -> Self {
        Self(errors.0.into_iter().map(|e| e.into()).collect())
    }
}

#[derive(Debug)]
pub enum ParserErrorType {
    MissingRightParen,
    ExpectedPrimaryExpression,
    ExpectedSemicolon,
    ExpectedIdentifier,
    InvalidAssignmentTarget,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self { tokens, current: 0.into() }
    }

    pub fn parse(&self) -> Result<Vec<Stmt>, RloxErrors> {
        let mut errors = ParserErrors(Vec::new());
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => errors.0.push(e),
            }
        }

        if errors.0.is_empty() {
            Ok(stmts)
        } else {
            Err(errors.into())
        }
    }

    fn declaration(&self) -> Result<Stmt, ParserError> {
        if self.consume(Var).is_some() {
            self.var_declaration()
        } else {
            self.statement().map_err(|e| {
                self.synchronize();
                e
            })
        }
    }

    fn var_declaration(&self) -> Result<Stmt, ParserError> {
        let Some(name) = self.consume(Identifier) else {
            return Err(ParserError::new(ParserErrorType::ExpectedIdentifier, self.previous()));
        };

        let initializer =
            if self.consume(Equal).is_some() { Some(self.expression()?) } else { None };

        if self.consume(Semicolon).is_some() {
            Ok(Stmt::Var { name, initializer })
        } else {
            Err(ParserError::new(ParserErrorType::ExpectedSemicolon, self.previous()))
        }
    }

    fn statement(&self) -> Result<Stmt, ParserError> {
        match self.peek() {
            Some(Print) => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&self) -> Result<Stmt, ParserError> {
        self.advance();
        let value = self.expression()?;
        if self.consume(Semicolon).is_some() {
            Ok(Stmt::Print(value))
        } else {
            Err(ParserError::new(ParserErrorType::ExpectedSemicolon, self.previous()))
        }
    }

    fn expression_statement(&self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        if self.consume(Semicolon).is_some() {
            Ok(Stmt::Expression(value))
        } else {
            Err(ParserError::new(ParserErrorType::ExpectedSemicolon, self.previous()))
        }
    }

    fn expression(&self) -> Result<Box<Expr>, ParserError> {
        self.assignment()
    }

    fn assignment(&self) -> Result<Box<Expr>, ParserError> {
        let expr = self.equality()?;

        if self.consume(Equal).is_some() {
            let value = self.assignment()?;

            if let Expr::Variable(name) = *expr {
                return Ok(Box::new(Expr::Assign { name, value }));
            }

            return Err(ParserError::new(
                ParserErrorType::InvalidAssignmentTarget,
                self.previous(),
            ));
        }

        Ok(expr)
    }

    fn equality(&self) -> Result<Box<Expr>, ParserError> {
        let mut expr = self.comparison()?;

        while let Some(BangEqual) | Some(EqualEqual) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Box::new(Expr::Binary { left: expr, operator, right })
        }
        Ok(expr)
    }

    fn comparison(&self) -> Result<Box<Expr>, ParserError> {
        let mut expr = self.term()?;

        while let Some(Greater) | Some(GreaterEqual) | Some(Less) | Some(LessEqual) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Box::new(Expr::Binary { left: expr, operator, right })
        }
        Ok(expr)
    }

    fn term(&self) -> Result<Box<Expr>, ParserError> {
        let mut expr = self.factor()?;

        while let Some(Plus) | Some(Minus) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Box::new(Expr::Binary { left: expr, operator, right })
        }
        Ok(expr)
    }

    fn factor(&self) -> Result<Box<Expr>, ParserError> {
        let mut expr = self.unary()?;

        while let Some(Star) | Some(Slash) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Box::new(Expr::Binary { left: expr, operator, right })
        }
        Ok(expr)
    }

    fn unary(&self) -> Result<Box<Expr>, ParserError> {
        if let Some(Minus) | Some(Bang) = self.peek() {
            self.advance();
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Box::new(Expr::Unary { operator, right }));
        }
        self.primary()
    }

    fn primary(&self) -> Result<Box<Expr>, ParserError> {
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

                if self.consume(RightParen).is_some() {
                    Ok(Box::new(Expr::Grouping(expr)))
                } else {
                    Err(ParserError::new(ParserErrorType::MissingRightParen, self.previous()))
                }
            }
            Some(Identifier) => {
                self.advance();
                Ok(Box::new(Expr::Variable(self.previous())))
            }
            _ => Err(ParserError::new(
                ParserErrorType::ExpectedPrimaryExpression,
                self.peek_token().unwrap(),
            )),
        }
    }

    fn consume(&self, token: TokenData) -> Option<&Token> {
        assert!(!matches!(token, Number(_) | Str(_)));
        match self.peek() {
            Some(t) if t == &token => Some(self.advance()),
            _ => None,
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

#[cfg(test)]
mod tests {
    use scanner::Scanner;

    use super::*;

    #[test]
    fn missing_semicolon() {
        let scanner = Scanner::new("var a = 1");
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let result = parser.parse();
        assert_eq!(
            result.unwrap_err(),
            RloxErrors(vec![RloxError {
                line: 1,
                col: 9,
                message: "Expected semicolon after 1".to_string()
            }])
        );
    }

    #[test]
    fn synchronize_after_error() {
        let scanner = Scanner::new("var a = 1 var b = 2;\nvar c = 3");
        let tokens = scanner.scan_tokens().unwrap();
        let parser = Parser::new(&tokens);
        let result = parser.parse();
        assert_eq!(
            result.unwrap_err(),
            RloxErrors(vec![
                RloxError { line: 1, col: 9, message: "Expected semicolon after 1".to_string() },
                RloxError { line: 2, col: 9, message: "Expected semicolon after 3".to_string() }
            ])
        );
    }
}

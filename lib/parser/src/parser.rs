mod expr;
mod stmt;
use errors::{RloxError, RloxErrors};
use std::{cell::RefCell, iter::Peekable, unreachable};
pub use stmt::Stmt;

pub use expr::{Expr, LiteralValue};
use scanner::{token::TokenData, Token, TokenStream};

use errors::Result;

use TokenData::*;

#[derive(Debug)]
pub struct ParserError<'a> {
    error: ParserErrorType,
    token: Token<'a>,
}

impl<'a> From<ParserError<'a>> for RloxError {
    fn from(error: ParserError<'a>) -> Self {
        RloxError {
            line: error.token.line(),
            col: error.token.col(),
            message: error.error.to_string(),
        }
    }
}

impl<'a> ParserError<'a> {
    fn new(error: ParserErrorType, token: Token<'a>) -> Self {
        Self { token, error }
    }
}

#[derive(Debug)]
pub enum ParserErrorType {
    MissingLeftParen,
    MissingRightParen,
    ExpectedPrimaryExpression,
    ExpectedSemicolon,
    ExpectedIdentifier,
    InvalidAssignmentTarget,
    ExpectedRightBrace,
    TooManyArguments,
}

impl std::fmt::Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ParserErrorType::MissingLeftParen => "Missing opening `(` before expression",
                ParserErrorType::MissingRightParen => "Missing closing `)` after expression",
                ParserErrorType::ExpectedPrimaryExpression => "Expected primary expression",
                ParserErrorType::ExpectedSemicolon => "Expected semicolon after expression",
                ParserErrorType::ExpectedIdentifier => "Expected identifier after expression",
                ParserErrorType::InvalidAssignmentTarget => "Invalid assignment target",
                ParserErrorType::ExpectedRightBrace => "Expected '}' after block",
                ParserErrorType::TooManyArguments => "Can't have more than 255 arguments",
            }
        )
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    token_stream: RefCell<Peekable<TokenStream<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn new(token_stream: TokenStream<'a>) -> Self {
        Self { token_stream: RefCell::new(token_stream.peekable()) }
    }

    pub fn parse(&'a self) -> std::result::Result<Vec<Stmt<'a>>, RloxErrors> {
        let mut errors = RloxErrors(Vec::new());
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.synchronize();
                    errors.0.push(e)
                }
            }
        }

        if errors.0.is_empty() {
            Ok(stmts)
        } else {
            Err(errors)
        }
    }

    fn declaration(&'a self) -> Result<Stmt<'a>> {
        if self.consume(Var)?.is_ok() {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&'a self) -> Result<Stmt<'a>> {
        let name = self.consume_or_error(Identifier, ParserErrorType::ExpectedIdentifier)?;

        let initializer = match self.consume(Equal)? {
            Ok(_) => Some(self.expression()?),
            Err(_) => None,
        };

        self.consume_or_error(Semicolon, ParserErrorType::ExpectedSemicolon)?;

        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&'a self) -> Result<Stmt<'a>> {
        if self.consume(Print)?.is_ok() {
            return self.print_statement();
        }

        if self.consume(LeftBrace)?.is_ok() {
            return self.block();
        }

        if self.consume(If)?.is_ok() {
            return self.if_statement();
        }

        if self.consume(While)?.is_ok() {
            return self.while_statement();
        }

        if self.consume(For)?.is_ok() {
            return self.for_statement();
        }

        self.expression_statement()
    }

    fn for_statement(&'a self) -> Result<Stmt<'a>> {
        self.consume_or_error(LeftParen, ParserErrorType::MissingLeftParen)?;

        let initializer = if self.consume(Semicolon)?.is_ok() {
            None
        } else if self.consume(Var).is_ok() {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if self.peek() == Some(Ok(Semicolon)) {
            Expr::Literal(LiteralValue::Boolean(true))
        } else {
            self.expression()?
        };
        self.consume_or_error(Semicolon, ParserErrorType::ExpectedSemicolon)?;

        let increment =
            if self.peek() == Some(Ok(RightParen)) { None } else { Some(self.expression()?) };
        self.consume_or_error(RightParen, ParserErrorType::MissingRightParen)?;

        let body = self.statement()?;

        let while_smt = Stmt::While {
            condition,
            body: Box::new(Stmt::Block(
                [Some(body), increment.map(Stmt::Expression)].into_iter().flatten().collect(),
            )),
        };

        Ok(Stmt::Block([initializer, Some(while_smt)].into_iter().flatten().collect()))
    }

    fn while_statement(&'a self) -> Result<Stmt<'a>> {
        self.consume_or_error(LeftParen, ParserErrorType::MissingLeftParen)?;
        let condition = self.expression()?;
        self.consume_or_error(RightParen, ParserErrorType::MissingRightParen)?;

        let body = Box::new(self.statement()?);

        Ok(Stmt::While { condition, body })
    }

    fn if_statement(&'a self) -> Result<Stmt<'a>> {
        self.consume_or_error(LeftParen, ParserErrorType::MissingLeftParen)?;
        let condition = self.expression()?;
        self.consume_or_error(RightParen, ParserErrorType::MissingRightParen)?;

        let then_branch = Box::new(self.statement()?);

        let else_branch = match self.consume(Else)? {
            Ok(_) => Some(self.statement()?),
            Err(_) => None,
        }
        .map(Box::new);

        Ok(Stmt::If { condition, then_branch, else_branch })
    }

    fn block(&'a self) -> Result<Stmt<'a>> {
        let mut stmts = Vec::new();

        loop {
            if self.peek() == Some(Ok(RightBrace)) {
                break;
            }
            stmts.push(self.declaration()?);
        }

        self.consume_or_error(RightBrace, ParserErrorType::ExpectedRightBrace)?;
        Ok(Stmt::Block(stmts))
    }

    fn print_statement(&'a self) -> Result<Stmt<'a>> {
        let value = self.expression()?;

        self.consume_or_error(Semicolon, ParserErrorType::ExpectedSemicolon)?;

        Ok(Stmt::Print(value))
    }

    fn expression_statement(&'a self) -> Result<Stmt<'a>> {
        let value = self.expression()?;

        self.consume_or_error(Semicolon, ParserErrorType::ExpectedSemicolon)?;

        Ok(Stmt::Expression(value))
    }

    fn expression(&'a self) -> Result<Expr<'a>> {
        self.assignment()
    }

    fn assignment(&'a self) -> Result<Expr<'a>> {
        let expr = self.or()?;

        if let Ok(equal) = self.consume(Equal)? {
            let value = Box::new(self.assignment()?);

            if let Expr::Variable(name) = expr {
                return Ok(Expr::Assign { name, value });
            }

            return Err(ParserError::new(ParserErrorType::InvalidAssignmentTarget, equal).into());
        }

        Ok(expr)
    }

    fn or(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.and()?;

        while let Ok(operator) = self.consume(Or)? {
            let right = Box::new(self.and()?);
            expr = Expr::Logical { left: Box::new(expr), operator, right };
        }

        Ok(expr)
    }

    fn and(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.equality()?;

        while let Ok(operator) = self.consume(And)? {
            let right = Box::new(self.equality()?);
            expr = Expr::Logical { left: Box::new(expr), operator, right };
        }

        Ok(expr)
    }

    fn equality(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.comparison()?;

        while let Some(Ok(BangEqual)) | Some(Ok(EqualEqual)) = self.peek() {
            let operator = self.advance()?;
            let right = Box::new(self.comparison()?);
            expr = Expr::Binary { left: Box::new(expr), operator, right }
        }
        Ok(expr)
    }

    fn comparison(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.term()?;

        while let Some(Ok(Greater))
        | Some(Ok(GreaterEqual))
        | Some(Ok(Less))
        | Some(Ok(LessEqual)) = self.peek()
        {
            let operator = self.advance()?;
            let right = Box::new(self.term()?);
            expr = Expr::Binary { left: Box::new(expr), operator, right }
        }
        Ok(expr)
    }

    fn term(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.factor()?;

        while let Some(Ok(Plus)) | Some(Ok(Minus)) = self.peek() {
            let operator = self.advance()?;
            let right = Box::new(self.factor()?);
            expr = Expr::Binary { left: Box::new(expr), operator, right }
        }
        Ok(expr)
    }

    fn factor(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.unary()?;

        while let Some(Ok(Star)) | Some(Ok(Slash)) = self.peek() {
            let operator = self.advance()?;
            let right = Box::new(self.unary()?);
            expr = Expr::Binary { left: Box::new(expr), operator, right }
        }
        Ok(expr)
    }

    fn unary(&'a self) -> Result<Expr<'a>> {
        if let Some(Ok(Minus)) | Some(Ok(Bang)) = self.peek() {
            let operator = self.advance()?;
            let right = Box::new(self.unary()?);
            return Ok(Expr::Unary { operator, right });
        }
        self.call()
    }

    fn call(&'a self) -> Result<Expr<'a>> {
        let mut expr = self.primary()?;

        loop {
            if self.consume(LeftParen)?.is_ok() {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&'a self, callee: Expr<'a>) -> Result<Expr<'a>> {
        let mut arguments = Vec::new();

        if self.peek() != Some(Ok(RightParen)) {
            loop {
                arguments.push(self.expression()?);

                if self.peek() != Some(Ok(Comma)) {
                    break;
                }

                // TODO jlox keeps parsing here instead of throwing an error.
                // We need a way to store errors w/o entering panic mode
                if arguments.len() >= 255 {
                    return Err(ParserError::new(
                        ParserErrorType::TooManyArguments,
                        self.peek_token().unwrap()?,
                    )
                    .into());
                }
            }
        }

        let closing_paren =
            self.consume_or_error(RightParen, ParserErrorType::MissingRightParen)?;

        Ok(Expr::Call { callee: Box::new(callee), closing_paren, arguments })
    }

    fn primary(&'a self) -> Result<Expr<'a>> {
        let token = self.advance()?;
        match token.data {
            False => Ok(Expr::Literal(LiteralValue::Boolean(false))),
            True => Ok(Expr::Literal(LiteralValue::Boolean(true))),
            Str(s) => Ok(Expr::Literal(LiteralValue::Str(s))),
            Number(n) => Ok(Expr::Literal(LiteralValue::Number(n))),
            Nil => Ok(Expr::Literal(LiteralValue::Nil)),
            LeftParen => {
                let expr = self.expression()?;

                self.consume_or_error(RightParen, ParserErrorType::MissingRightParen)?;

                Ok(Expr::Grouping(Box::new(expr)))
            }
            Identifier => Ok(Expr::Variable(token)),

            _ => Err(ParserError::new(ParserErrorType::ExpectedPrimaryExpression, token).into()),
        }
    }

    fn consume(&self, token: TokenData) -> Result<std::result::Result<Token, Token>> {
        assert!(!matches!(token, Number(_) | Str(_)));
        match self.peek_token() {
            Some(Ok(t)) if t.data == token => Ok(Ok(self.advance().unwrap())),
            Some(Ok(t)) => Ok(Err(t)),
            Some(Err(err)) => Err(err),
            None => unreachable!("Should have hit Eof"),
        }
    }

    fn consume_or_error(&self, token: TokenData, error_type: ParserErrorType) -> Result<Token> {
        match self.consume(token)? {
            Ok(token) => Ok(token),
            Err(token) => Err(ParserError::new(error_type, token).into()),
        }
    }

    fn synchronize(&self) {
        loop {
            if let Ok(t) = self.advance() {
                match t.data {
                    Semicolon => return,
                    Class | Fun | Var | For | If | While | Print | Return | Eof => return,
                    _ => {}
                }
            }
        }
    }
}

// Helpers
impl<'a> Parser<'a> {
    fn peek_token(&self) -> Option<Result<Token>> {
        self.token_stream.borrow_mut().peek().cloned()
    }

    fn peek(&self) -> Option<Result<TokenData>> {
        self.peek_token().map(|t| t.map(|t| t.data))
    }

    fn advance(&self) -> Result<Token<'a>> {
        self.token_stream.borrow_mut().next().unwrap()
    }

    fn is_at_end(&self) -> bool {
        match self.peek() {
            Some(Ok(Eof)) => true,
            Some(_) => false,
            None => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use cursor::{Col, Line};
    use scanner::ScanError;

    use super::*;

    #[test]
    fn invalid_ident() {
        let parser = Parser::new(TokenStream::new("var @ = 3;\nprint $<4;"));
        let result = parser.parse();
        assert_eq!(
            result.unwrap_err(),
            RloxErrors(vec![
                RloxError {
                    line: Line(1),
                    col: Col(5),
                    message: ScanError::UnexpectedCharacter('@').to_string(),
                },
                RloxError {
                    line: Line(2),
                    col: Col(7),
                    message: ScanError::UnexpectedCharacter('$').to_string(),
                }
            ])
        );
    }

    #[test]
    fn print_without_semicolon() {
        let parser = Parser::new(TokenStream::new("print 1"));
        let result = parser.parse();
        assert_eq!(
            result.unwrap_err(),
            RloxErrors(vec![RloxError {
                line: Line(1),
                col: Col(8),
                message: ParserErrorType::ExpectedSemicolon.to_string(),
            }])
        );
    }

    #[test]
    fn print_invalid_identifier() {
        let parser = Parser::new(TokenStream::new("print @;"));
        let result = parser.parse();
        assert_eq!(
            result.unwrap_err(),
            RloxErrors(vec![RloxError {
                line: Line(1),
                col: Col(7),
                message: scanner::ScanError::UnexpectedCharacter('@').to_string(),
            }])
        );
    }

    #[test]
    fn missing_semicolon() {
        let parser = Parser::new(TokenStream::new("var a = 1"));
        let result = parser.parse();
        assert_eq!(
            result.unwrap_err(),
            RloxErrors(vec![RloxError {
                line: Line(1),
                col: Col(10),
                message: ParserErrorType::ExpectedSemicolon.to_string(),
            }])
        );
    }

    #[test]
    fn synchronize_after_error() {
        let parser = Parser::new(TokenStream::new("var a = 1 var b = 2;\nvar c = 3"));
        let result = parser.parse();
        assert_eq!(
            result.unwrap_err(),
            RloxErrors(vec![
                RloxError {
                    line: Line(1),
                    col: Col(11),
                    message: ParserErrorType::ExpectedSemicolon.to_string()
                },
                RloxError {
                    line: Line(2),
                    col: Col(10),
                    message: ParserErrorType::ExpectedSemicolon.to_string()
                },
            ])
        );
    }
}

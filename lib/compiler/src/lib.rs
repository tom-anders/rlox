use std::{cell::RefCell, debug_assert, iter::Peekable, unreachable};

use bytecode::{
    chunk::Chunk,
    instructions::{Instruction, OpCode},
    value::Value,
};
use cursor::Line;
use errors::{RloxError, RloxErrors};
use log::{debug, info, trace};
use scanner::{token::TokenData, Token, TokenStream};

use TokenData::*;

pub type Result<T> = std::result::Result<T, RloxError>;

#[derive(Debug)]
pub struct CompilerError<'a> {
    error: CompilerErrorType,
    token: Token<'a>,
}

impl<'a> From<CompilerError<'a>> for RloxError {
    fn from(error: CompilerError<'a>) -> Self {
        RloxError {
            line: error.token.line(),
            col: error.token.col(),
            message: error.error.to_string(),
        }
    }
}

impl<'a> CompilerError<'a> {
    fn new(error: CompilerErrorType, token: Token<'a>) -> Self {
        Self { token, error }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CompilerErrorType {
    #[error("Too many constants")]
    TooManyConstants,
    #[error("Expected EOF")]
    ExpectedEof,
    #[error("Expected ')' after expression")]
    ExpectedRightParen,
    #[error("Expected expression")]
    ExpectedExpression,
}

#[repr(u8)]
#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    num_enum::IntoPrimitive,
    num_enum::TryFromPrimitive,
)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    pub fn next_higher_precedence(self) -> Self {
        let prim: u8 = self.into();
        (prim + 1).try_into().unwrap()
    }

    pub fn next_lower_precedence(self) -> Self {
        let prim: u8 = self.into();
        (prim - 1).try_into().unwrap()
    }
}

#[derive(Debug)]
pub struct Compiler<'a> {
    // TODO should be able to get rid of the RefCell here
    token_stream: RefCell<Peekable<TokenStream<'a>>>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { token_stream: RefCell::new(TokenStream::new(source).peekable()) }
    }

    pub fn compile(self) -> std::result::Result<Chunk, RloxErrors> {
        let mut errors = RloxErrors(Vec::new());
        let mut chunk = Chunk::default();

        while !self.is_at_end() {
            match self.expression(&mut chunk) {
                Ok(()) => (),
                Err(e) => {
                    log::trace!("Hit error: {:?}, syncing...", e);
                    self.synchronize();
                    errors.0.push(e)
                }
            }
        }

        match self.consume_or_error(TokenData::Eof, CompilerErrorType::ExpectedEof) {
            Ok(_) => {}
            Err(e) => errors.0.push(e),
        }

        // Only temporarily so that in the VM we get some output
        chunk.write_instruction(Instruction::Return, Line(0));

        if errors.is_empty() {
            log::trace!("Compiled chunk: {chunk:?}");
            Ok(chunk)
        } else {
            Err(errors)
        }
    }

    fn expression(&self, chunk: &mut Chunk) -> Result<()> {
        self.parse_precedence(chunk, Precedence::Assignment)
    }

    fn number(&self, chunk: &mut Chunk, prefix_token: &Token<'a>) -> Result<()> {
        trace!("Compiling number: {:?}", prefix_token);
        let value = prefix_token.lexeme().parse::<f64>().unwrap();
        let instr = chunk.add_constant(Value::Number(value)).ok_or_else(|| {
            CompilerError::new(CompilerErrorType::TooManyConstants, prefix_token.clone())
        })?;
        chunk.write_instruction(instr, prefix_token.line());
        Ok(())
    }

    fn grouping(&self, chunk: &mut Chunk, _: &Token<'a>) -> Result<()> {
        self.expression(chunk)?;
        self.consume_or_error(TokenData::RightParen, CompilerErrorType::ExpectedRightParen)?;
        Ok(())
    }

    fn unary(&self, chunk: &mut Chunk, prefix_token: &Token<'a>) -> Result<()> {
        // Compile the operand
        self.parse_precedence(chunk, Precedence::Unary)?;
        chunk.write_instruction(Instruction::Negate, prefix_token.line());
        Ok(())
    }

    fn binary(&self, chunk: &mut Chunk, operator_token: &Token<'a>) -> Result<()> {
        self.parse_precedence(
            chunk,
            self.token_precendence(operator_token).next_higher_precedence(),
        )?;

        match operator_token.data {
            Minus => chunk.write_instruction(Instruction::Subtract, operator_token.line()),
            Plus => chunk.write_instruction(Instruction::Add, operator_token.line()),
            Slash => chunk.write_instruction(Instruction::Divide, operator_token.line()),
            Star => chunk.write_instruction(Instruction::Multiply, operator_token.line()),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn advance_with_prefix_rule(&self, chunk: &mut Chunk) -> Result<()> {
        let token = self.advance()?;
        match token.data {
            Number(_) => self.number(chunk, &token),
            LeftParen => self.grouping(chunk, &token),
            Minus => self.unary(chunk, &token),
            _ => {
                Err(CompilerError::new(CompilerErrorType::ExpectedExpression, token.clone()).into())
            }
        }
    }

    fn advance_with_infix_rule(&self, chunk: &mut Chunk) -> Result<()> {
        let token = self.advance()?;
        match token.data {
            Plus | Minus | Slash | Star => self.binary(chunk, &token),
            _ => {
                Err(CompilerError::new(CompilerErrorType::ExpectedExpression, token.clone()).into())
            }
        }
    }

    fn token_precendence(&self, token: &Token) -> Precedence {
        match token.data {
            Plus | Minus => Precedence::Term,
            Slash | Star => Precedence::Factor,
            _ => Precedence::None,
        }
    }

    fn parse_precedence(&self, chunk: &mut Chunk, precedence: Precedence) -> Result<()> {
        self.advance_with_prefix_rule(chunk)?;

        while precedence <= self.token_precendence(&self.peek_token().unwrap()?) {
            self.advance_with_infix_rule(chunk)?;
        }

        Ok(())
    }

    fn synchronize(&self) {
        loop {
            log::trace!("Syncing... {:?}", self.peek_token());
            match self.peek_token() {
                Some(Ok(t)) => match t.data {
                    Semicolon => {
                        self.advance().unwrap();
                        return;
                    }
                    Class | Fun | Var | For | If | While | Print | Return | Eof => return,
                    _ => {}
                },
                Some(Err(_)) => (),
                None => return,
            }
            self.advance().unwrap();
        }
    }
}

// Helpers
impl<'a> Compiler<'a> {
    fn consume(&self, token: TokenData) -> Result<std::result::Result<Token, Token>> {
        debug_assert!(!matches!(token, Number(_) | Str(_)));
        match self.peek_token() {
            Some(Ok(t)) if t.data == token => Ok(Ok(self.advance().unwrap())),
            Some(Ok(t)) => Ok(Err(t)),
            Some(Err(err)) => Err(err),
            None => unreachable!("Should have hit Eof"),
        }
    }

    fn consume_or_error(&self, token: TokenData, error: CompilerErrorType) -> Result<Token> {
        match self.consume(token)? {
            Ok(token) => Ok(token),
            Err(token) => Err(CompilerError::new(error, token).into()),
        }
    }

    fn peek_token(&self) -> Option<Result<Token<'a>>> {
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

use std::{cell::RefCell, debug_assert, iter::Peekable, unreachable, rc::Rc};

use bytecode::{
    chunk::Chunk,
    instructions::{Instruction, OpCode},
    value::{Value, Object, ObjectData},
};
use cursor::{Line, Col};
use errors::{RloxError, RloxErrors};
use log::{debug, info, trace};
use scanner::{token::TokenData, Token, TokenStream, TokenType};

use TokenType::*;

pub type Result<T> = std::result::Result<T, RloxError>;

#[derive(Debug)]
pub struct CompilerError {
    error: CompilerErrorType,
    line: Line,
    col: Col,
}

impl From<CompilerError> for RloxError {
    fn from(error: CompilerError) -> Self {
        RloxError {
            line: error.line,
            col: error.col,
            message: error.error.to_string(),
        }
    }
}

impl CompilerError {
    fn new(error: CompilerErrorType, token: Token) -> Self {
        Self { error, line: token.line(), col: token.col() }
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
    #[error("Expected ';'")]
    ExpectedSemicolon,
    #[error("Expected variable name")]
    ExpectedVariableName,
    #[error("Invalid assignment target")]
    InvalidAssignmentTarget,
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

    pub fn lower_or_equal(self, other: Self) -> bool {
        self as u8 <= other as u8
    }
}

#[derive(Debug)]
pub struct Compiler<'a> {
    token_stream: Peekable<TokenStream<'a>>,
    chunk: Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { token_stream: TokenStream::new(source).peekable(), chunk: Chunk::default() }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    pub fn compile(mut self) -> std::result::Result<Chunk, RloxErrors> {
        let mut errors = RloxErrors(Vec::new());

        while !self.is_at_end() {
            match self.declaration() {
                Ok(()) => (),
                Err(e) => {
                    log::trace!("Hit error: {:?}, syncing...", e);
                    self.synchronize();
                    errors.0.push(e)
                }
            }
        }

        match self.consume_or_error(Eof, CompilerErrorType::ExpectedEof) {
            Ok(_) => {}
            Err(e) => errors.0.push(e),
        }

        // Only temporarily so that in the VM we get some output
        self.current_chunk().write_instruction(Instruction::Return, Line(0));

        if errors.is_empty() {
            log::trace!("Compiled chunk: {:?}", self.current_chunk());
            Ok(self.chunk)
        } else {
            Err(errors)
        }
    }

    fn declaration(&mut self) -> Result<()> {
        if self.consume(Var)?.is_ok() {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn variable(&mut self, token: &Token<'a>, can_assign: bool) -> Result<()> {
        self.named_variable(token, can_assign)
    }

    fn named_variable(&mut self, identifier: &Token<'a>, can_assign: bool) -> Result<()> {
        let constant_index = self.add_identifier_constant(identifier)?;
        if can_assign && self.consume(Equal)?.is_ok() {
            self.expression()?;
            self.chunk.write_instruction(Instruction::SetGlobal { constant_index }, identifier.line());
        } else {
            self.chunk.write_instruction(Instruction::ReadGlobal{ constant_index }, identifier.line());
        }
        Ok(())
    }

    fn var_declaration(&mut self) -> Result<()> {
        let (global, line) = self.parse_variable(CompilerErrorType::ExpectedVariableName)?;
        if self.consume(Equal)?.is_ok() {
            self.expression()?;
        } else {
            self.chunk.write_instruction(Instruction::Nil, line);
        }
        self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?;
        self.define_variable(global, line)
    }

    fn parse_variable(&mut self, error: CompilerErrorType) -> Result<(u8, Line)> {
        let (name, line) = match self.consume_or_error(Identifier, error) {
            Ok(token) => (token.lexeme().to_string(), token.line()),
            Err(e) => return Err(e),
        };

        let index = self.chunk.add_constant(Value::string(name))
            .ok_or(CompilerError { error: CompilerErrorType::TooManyConstants, line, col: Col(1) })?;

        Ok((index, line))
    }

    fn add_identifier_constant(&mut self, token: &Token<'a>) -> Result<u8> {
        self.current_chunk().add_constant(Value::string(token.lexeme().to_string()))
            .ok_or_else(|| CompilerError::new(CompilerErrorType::TooManyConstants, token.clone()).into())
    }

    fn define_variable(&mut self, global: u8, line: Line) -> Result<()> {
        self.chunk.write_instruction(Instruction::DefineGlobal{ constant_index: global }, line);
        Ok(())
    }

    fn statement(&mut self) -> Result<()> {
        if self.consume(Print)?.is_ok() {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }
    
    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        let line = self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?.line();
        self.chunk.write_instruction(Instruction::Print, line);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        let line = self.consume_or_error(Semicolon, CompilerErrorType::ExpectedSemicolon)?.line();
        self.chunk.write_instruction(Instruction::Pop, line);
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn literal(&mut self, prefix_token: &Token<'a>) -> Result<()> {
        trace!("Compiling literal: {:?}", prefix_token);
        self.chunk.write_instruction(match prefix_token.ty() {
            True => Instruction::True,
            False => Instruction::False,
            Nil => Instruction::Nil,
            _ => unreachable!(),
        }, prefix_token.line());

        Ok(())
    }

    fn number(&mut self, prefix_token: &Token<'a>) -> Result<()> {
        trace!("Compiling number: {:?}", prefix_token);
        let value = match prefix_token.data {
            TokenData::Number(n) => n,
            _ => unreachable!(),
        };
        let index = self.current_chunk().add_constant(Value::Number(value)).ok_or_else(|| {
            CompilerError::new(CompilerErrorType::TooManyConstants, prefix_token.clone())
        })?;
        self.current_chunk().write_instruction(Instruction::Constant { index }, prefix_token.line());
        Ok(())
    }

    fn string(&mut self, prefix_token: &Token<'a>) -> Result<()> {
        let s = match prefix_token.data {
            TokenData::Str(s) => s,
            _ => unreachable!(),
        }.to_string();

        let index = self.current_chunk().add_constant(Value::Object(Box::new(Object::new(ObjectData::String(Rc::new(s)))))).ok_or_else(|| {
            CompilerError::new(CompilerErrorType::TooManyConstants, prefix_token.clone())
        })?;
        self.current_chunk().write_instruction(Instruction::Constant { index }, prefix_token.line());

        Ok(())
    }

    fn grouping(&mut self, _: &Token<'a>) -> Result<()> {
        self.expression()?;
        self.consume_or_error(RightParen, CompilerErrorType::ExpectedRightParen)?;
        Ok(())
    }

    fn unary(&mut self, prefix_token: &Token<'a>) -> Result<()> {
        // Compile the operand
        self.parse_precedence(Precedence::Unary)?;
        self.current_chunk().write_instruction(match prefix_token.ty() {
            Minus => Instruction::Negate,
            Bang => Instruction::Not,
            _ => unreachable!(),
        }, prefix_token.line());
        Ok(())
    }

    fn binary(&mut self, operator_token: &Token<'a>) -> Result<()> {
        let next_higher_prec = 
            self.token_precendence(operator_token.ty()).next_higher_precedence();
        self.parse_precedence(next_higher_prec)?;

        match operator_token.ty() {
            Minus => self.current_chunk().write_instruction(Instruction::Subtract, operator_token.line()),
            Plus => self.current_chunk().write_instruction(Instruction::Add, operator_token.line()),
            Slash => self.current_chunk().write_instruction(Instruction::Divide, operator_token.line()),
            Star => self.current_chunk().write_instruction(Instruction::Multiply, operator_token.line()),
            BangEqual => self.current_chunk().write_instructions([Instruction::Equal, Instruction::Not], operator_token.line()),
            EqualEqual => self.current_chunk().write_instruction(Instruction::Equal, operator_token.line()),
            Greater => self.current_chunk().write_instruction(Instruction::Greater, operator_token.line()),
            GreaterEqual => self.current_chunk().write_instructions([Instruction::Less, Instruction::Not], operator_token.line()),
            Less => self.current_chunk().write_instruction(Instruction::Less, operator_token.line()),
            LessEqual => self.current_chunk().write_instructions([Instruction::Greater, Instruction::Not], operator_token.line()),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn advance_with_prefix_rule(&mut self, can_assign: bool) -> Result<()> {
        let token = self.advance()?;
        trace!("Advancing with prefix rule for {}", token);
        match token.ty() {
            Number => self.number(&token),
            True | False | Nil => self.literal(&token),
            LeftParen => self.grouping(&token),
            Minus | Bang => self.unary(&token),
            Str => self.string(&token),
            Identifier => self.variable(&token, can_assign),
            _ => {
                Err(CompilerError::new(CompilerErrorType::ExpectedExpression, token.clone()).into())
            }
        }
    }

    fn advance_with_infix_rule(&mut self) -> Result<()> {
        let token = self.advance()?;
        trace!("Advancing with infix rule for {}", token);
        match token.ty() {
            Plus | Minus | Slash | Star |
            EqualEqual | BangEqual | Greater | GreaterEqual | Less | LessEqual => {
                self.binary(&token)
            }
            _ => {
                Err(CompilerError::new(CompilerErrorType::ExpectedExpression, token.clone()).into())
            }
        }
    }

    fn token_precendence(&mut self, token_type: TokenType) -> Precedence {
        match token_type {
            Plus | Minus => Precedence::Term,
            Slash | Star => Precedence::Factor,
            EqualEqual | BangEqual => Precedence::Equality,
            Greater | GreaterEqual | Less | LessEqual => Precedence::Comparison,
            _ => Precedence::None,
        }
    }

    fn peek_precendence(&mut self) -> Result<Precedence> {
        let peek = self.peek().unwrap()?;
        Ok(self.token_precendence(peek))
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        let can_assign = precedence.lower_or_equal(Precedence::Assignment);
        trace!("Parsing precedence: {:?}, can_assign: {can_assign}", precedence);
        self.advance_with_prefix_rule(can_assign)?;

        while precedence <= self.peek_precendence()? {
            self.advance_with_infix_rule()?;
        }

        if can_assign {
            // If the equal has not been consumed, we tried assigning to an invalid target,
            // so report that error here
            if let Ok(token) = self.consume(Equal)? {
                return Err(CompilerError::new(CompilerErrorType::InvalidAssignmentTarget, token).into())
            }
        }

        Ok(())
    }

    fn synchronize(&mut self) {
        loop {
            log::trace!("Syncing... {:?}", self.peek_token());
            match self.peek_token() {
                Some(Ok(t)) => match t.ty() {
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
    fn consume(&mut self, token_type: TokenType) -> Result<std::result::Result<Token, Token>> {
        match self.peek_token() {
            Some(Ok(t)) if t.ty() == token_type => Ok(Ok(self.advance().unwrap())),
            Some(Ok(t)) => Ok(Err(t)),
            Some(Err(err)) => Err(err),
            None => unreachable!("Should have hit Eof"),
        }
    }

    fn consume_or_error(&mut self, token_type: TokenType, error: CompilerErrorType) -> Result<Token> {
        match self.consume(token_type)? {
            Ok(token) => Ok(token),
            Err(token) => Err(CompilerError::new(error, token).into()),
        }
    }

    fn peek_token(&mut self) -> Option<Result<Token<'a>>> {
        self.token_stream.peek().cloned()
    }

    fn peek(&mut self) -> Option<Result<TokenType>> {
        self.peek_token().map(|t| t.map(|t| t.data.into()))
    }

    fn advance(&mut self) -> Result<Token<'a>> {
        self.token_stream.next().unwrap()
    }

    fn is_at_end(&mut self) -> bool {
        match self.peek() {
            Some(Ok(Eof)) => true,
            Some(_) => false,
            None => true,
        }
    }
}

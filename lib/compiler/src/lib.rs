use std::{cell::RefCell, debug_assert, iter::Peekable, unreachable};

use bytecode::{
    chunk::Chunk,
    instructions::{Instruction, OpCode},
};
use cursor::Line;
use errors::{RloxError, RloxErrors};
use scanner::{token::TokenData, Token, TokenStream};

use TokenData::*;

pub type Result<T> = std::result::Result<T, RloxError>;

#[derive(Debug)]
pub struct Compiler<'a> {
    token_stream: RefCell<Peekable<TokenStream<'a>>>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            token_stream: RefCell::new(TokenStream::new(source).peekable()),
        }
    }

    pub fn compile(self) -> std::result::Result<Chunk, RloxErrors> {
        let mut errors = RloxErrors(Vec::new());
        let mut chunk = Chunk::default();

        while !self.is_at_end() {
            match self.expression(&mut chunk) {
                Ok(()) => (),
                Err(e) => {
                    self.synchronize();
                    errors.0.push(e)
                }
            }
        }

        match self.consume_or_error(TokenData::Eof, "Expected end of expression") {
            Ok(_) => {}
            Err(e) => errors.0.push(e),
        }

        // Only temporarily so that in the VM we get some output
        chunk.write_instruction(Instruction::Return, Line(0));

        if errors.is_empty() {
            Ok(chunk)
        } else {
            Err(errors)
        }
    }

    fn expression(&self, chunk: &mut Chunk) -> Result<()> {
        todo!()
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

    fn consume_or_error(&self, token: TokenData, msg: &'static str) -> Result<Token> {
        match self.consume(token)? {
            Ok(token) => Ok(token),
            Err(token) => {
                Err(RloxError { line: token.line(), col: token.col(), message: msg.to_string() })
            }
        }
    }

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

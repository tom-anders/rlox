use std::{fmt::Display};

use source::Cursor;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub data: TokenData<'a>,
    pub start: Cursor<'a>,
    pub end: Cursor<'a>,
    // pub lexeme: &'a str,
    // pub line: usize,
    // pub col_start: usize,
}

impl<'a> Token<'a> {
    // TODO store the cursors/range instead?
    pub fn new(data: TokenData<'a>, start: Cursor<'a>, end: Cursor<'a>) -> Token<'a> {
        Self {
            data,
            start,
            end,
        }
    }

    pub fn lexeme(&self) -> &str {
        self.start.slice_until(&self.end)
    }

    pub fn line(&self) -> usize {
        self.start.line()
    }

    pub fn col(&self) -> usize {
        self.start.col()
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lexeme())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenData<'a> {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    Str(&'a str),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}


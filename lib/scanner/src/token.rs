use std::fmt::Display;

use cursor::{Col, Cursor, Line, SourceRange};

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub data: TokenData<'a>,
    pub range: SourceRange<'a>,
}

impl<'a> Token<'a> {
    pub fn new(data: TokenData<'a>, range: impl Into<SourceRange<'a>>) -> Token<'a> {
        Self { data, range: range.into() }
    }

    pub fn ty(&self) -> TokenType {
        self.data.into()
    }

    pub fn lexeme(&self) -> &'a str {
        self.range.lexeme()
    }

    pub fn line(&self) -> Line {
        self.range.line()
    }

    pub fn col(&self) -> Col {
        self.range.col()
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lexeme())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, strum::EnumDiscriminants)]
#[strum_discriminants(name(TokenType))]
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

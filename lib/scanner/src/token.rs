#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub data: TokenData,
    pub lexeme: String,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenData {
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
    StringLiteral(String),
    Number(f64),

    // Keywords.
    And,
    Claas,
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


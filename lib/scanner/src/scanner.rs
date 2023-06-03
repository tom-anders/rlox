use std::{fmt::Display, println, todo, write};

use itertools::Itertools;

#[derive(thiserror::Error, Debug, PartialEq)]
enum Error {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacted(char),
    #[error("Unterminated string: {0}")]
    UnterminatedString(String),
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub struct Errors(Vec<Error>);

impl Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|e| e.to_string()).join("\n"))
    }
}

pub mod token;
pub use token::*;
use token::TokenData::{self, *};

pub struct Scanner<'a> {
    start: std::str::Chars<'a>,
    line: usize,
    iter: std::str::Chars<'a>,
    tokens: Vec<Token<'a>>,
    errors: Errors,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: source.chars(),
            line: 1,
            iter: source.chars(),
            tokens: Vec::new(),
            errors: Errors(Vec::new()),
        }
    }

    fn add_token(&mut self, data: TokenData<'a>) {
        let start = self.start.as_str();
        self.tokens.push(Token {
            data,
            // https://wduquette.github.io/parsing-strings-into-slices/
            // TODO factor out into helper
            lexeme: &start[..start.len() - self.iter.as_str().len()],
            line: self.line,
        })
    }

    fn peek_next(&self) -> Option<char> {
        let mut tmp = self.iter.clone();
        match tmp.next() {
            Some(_) => tmp.next(),
            None => None,
        }
    }

    fn peek(&self) -> Option<char> {
        self.iter.clone().next()
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.iter.next();
        if c == Some('\n') {
            self.line += 1;
        }
        c
    }

    fn try_consume(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.consume();
            true
        } else {
            false
        }
    }

    fn consume_until(&mut self, stop: char) -> bool {
        loop {
            match self.consume() {
                Some(c) if c == stop => return true,
                Some('\n') => return false,
                None => return false,
                _ => (),
            };
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token<'a>>, Errors> {
        loop {
            self.start = self.iter.clone();
            if let Some(c) = self.consume() {
                match c {
                    '(' => self.add_token(LeftParen),
                    ')' => self.add_token(RightParen),
                    '{' => self.add_token(LeftBrace),
                    '}' => self.add_token(RightBrace),
                    ',' => self.add_token(Comma),
                    '.' => self.add_token(Dot),
                    '-' => self.add_token(Minus),
                    '+' => self.add_token(Plus),
                    ';' => self.add_token(Semicolon),
                    '*' => self.add_token(Star),

                    '!' => {
                        if self.try_consume('=') {
                            self.add_token(BangEqual)
                        } else {
                            self.add_token(Bang)
                        }
                    }

                    '=' => {
                        if self.try_consume('=') {
                            self.add_token(EqualEqual)
                        } else {
                            self.add_token(Equal)
                        }
                    }

                    '<' => {
                        if self.try_consume('=') {
                            self.add_token(LessEqual)
                        } else {
                            self.add_token(Less)
                        }
                    }

                    '>' => {
                        if self.try_consume('=') {
                            self.add_token(GreaterEqual)
                        } else {
                            self.add_token(Greater)
                        }
                    }

                    '/' => {
                        if self.try_consume('/') {
                            // Comment
                            self.consume_until('\n');
                        } else if self.try_consume('*') {
                            self.block_comment();
                        } else {
                            self.add_token(Slash)
                        }
                    }

                    d if d.is_ascii_digit() => self.number(),

                    '"' => self.string(),

                    a if Self::is_alpha_or_underscore(a) => self.identifier(),

                    ' ' | '\r' | '\t' | '\n' => (),

                    c => self.errors.0.push(Error::UnexpectedCharacted(c)),
                }
            } else {
                break;
            }
        }
        self.tokens.push(Token {
            data: Eof,
            lexeme: "",
            line: self.line,
        });

        self.errors
            .0
            .is_empty()
            .then_some(self.tokens)
            .ok_or(self.errors)
    }

    fn is_alpha_or_underscore(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_alphanumeric_or_underscore(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn block_comment(&mut self) {
        let mut nest_level = 1;

        loop {
            match self.consume() {
                Some('*') if self.peek() == Some('/') => {
                    if nest_level == 1 {
                        self.consume();
                        return;
                    } else {
                        nest_level -= 1;
                    }
                }
                Some('/') if self.peek() == Some('*') => {
                    self.consume();
                    nest_level += 1;
                }
                _ => (),
            }
        }
    }

    fn string(&mut self) {
        let start = self.start.as_str();
        if !self.consume_until('"') {
            self.errors.0.push(Error::UnterminatedString(
                start[1..start.len() - self.iter.as_str().len()].to_string()
            ));
        } else {
            self.add_token(Str(
                &start[1..start.len() - self.iter.as_str().len() - 1],
            ));
        }
    }

    fn consume_digits(&mut self) {
        while self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            self.consume();
        }
    }

    fn number(&mut self) {
        self.consume_digits();

        if self.peek() == Some('.') && self.peek_next().map(|c| c.is_ascii_digit()) == Some(true) {
            self.consume();

            self.consume_digits();
        }

        let start = self.start.as_str();
        self.add_token(Number(
                start[..start.len() - self.iter.as_str().len()].parse().unwrap(),
        ));
    }

    fn identifier(&mut self) {
        while self
            .peek()
            .map(Self::is_alphanumeric_or_underscore)
            .unwrap_or(false)
        {
            self.consume();
        }

        let start = self.start.as_str();
        let text: String = start[..start.len() - self.iter.as_str().len()].to_string();

        match text.as_str() {
            "and" => self.add_token(And),
            "class" => self.add_token(Class),
            "else" => self.add_token(Else),
            "false" => self.add_token(False),
            "for" => self.add_token(For),
            "fun" => self.add_token(Fun),
            "if" => self.add_token(If),
            "nil" => self.add_token(Nil),
            "or" => self.add_token(Or),
            "print" => self.add_token(Print),
            "return" => self.add_token(Return),
            "super" => self.add_token(Super),
            "this" => self.add_token(This),
            "true" => self.add_token(True),
            "var" => self.add_token(Var),
            "while" => self.add_token(While),
            _ => self.add_token(Identifier),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eof(line: usize) -> Token<'static> {
        Token {
            data: Eof,
            lexeme: "",
            line,
        }
    }

    #[test]
    fn test_block_comments() {
        let source = "/* hello world */";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(tokens, vec![eof(1)]);

        let source = "/* hello world */ 123";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Number(123.0),
                    lexeme: "123",
                    line: 1,
                },
                eof(1),
            ]
        );

        let source = "/* hello world \n */ 123";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Number(123.0),
                    lexeme: "123",
                    line: 2,
                },
                eof(2),
            ]
        );

        // test nested block comments
        let source = "/* hello /* world */ */ 123";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Number(123.0),
                    lexeme: "123",
                    line: 1,
                },
                eof(1),
            ]
        );
    }

    #[test]
    fn test_number_literals() {
        let source = "123 1.23 1";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Number(123.0),
                    lexeme: "123",
                    line: 1,
                },
                Token {
                    data: Number(1.23),
                    lexeme: "1.23",
                    line: 1,
                },
                Token {
                    data: Number(1.0),
                    lexeme: "1",
                    line: 1,
                },
                eof(1),
            ]
        );
    }

    #[test]
    fn string_literals() {
        let source = "\"hello world\"";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Str("hello world"),
                    lexeme: "\"hello world\"",
                    line: 1,
                },
                eof(1),
            ]
        );

        let source = "\"hello world";
        let scanner = Scanner::new(source);
        let errors = scanner.scan_tokens().unwrap_err();
        assert_eq!(
            errors,
            Errors(vec![Error::UnterminatedString("hello world".to_string())])
        );

        let source = "\"hello world\n";
        let scanner = Scanner::new(source);
        let errors = scanner.scan_tokens().unwrap_err();
        assert_eq!(
            errors,
            Errors(vec![Error::UnterminatedString("hello world\n".to_string())])
        );
    }

    #[test]
    fn two_char_tokens() {
        let source = "! != = == < <= > >=";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Bang,
                    lexeme: "!",
                    line: 1,
                },
                Token {
                    data: BangEqual,
                    lexeme: "!=",
                    line: 1,
                },
                Token {
                    data: Equal,
                    lexeme: "=",
                    line: 1,
                },
                Token {
                    data: EqualEqual,
                    lexeme: "==",
                    line: 1,
                },
                Token {
                    data: Less,
                    lexeme: "<",
                    line: 1,
                },
                Token {
                    data: LessEqual,
                    lexeme: "<=",
                    line: 1,
                },
                Token {
                    data: Greater,
                    lexeme: ">",
                    line: 1,
                },
                Token {
                    data: GreaterEqual,
                    lexeme: ">=",
                    line: 1,
                },
                eof(1),
            ]
        );
    }

    #[test]
    fn test_single_char_tokens() {
        let source = "=(){},.-+;*/!<>";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Equal,
                    lexeme: "=",
                    line: 1,
                },
                Token {
                    data: LeftParen,
                    lexeme: "(",
                    line: 1,
                },
                Token {
                    data: RightParen,
                    lexeme: ")",
                    line: 1,
                },
                Token {
                    data: LeftBrace,
                    lexeme: "{",
                    line: 1,
                },
                Token {
                    data: RightBrace,
                    lexeme: "}",
                    line: 1,
                },
                Token {
                    data: Comma,
                    lexeme: ",",
                    line: 1,
                },
                Token {
                    data: Dot,
                    lexeme: ".",
                    line: 1,
                },
                Token {
                    data: Minus,
                    lexeme: "-",
                    line: 1,
                },
                Token {
                    data: Plus,
                    lexeme: "+",
                    line: 1,
                },
                Token {
                    data: Semicolon,
                    lexeme: ";",
                    line: 1,
                },
                Token {
                    data: Star,
                    lexeme: "*",
                    line: 1,
                },
                Token {
                    data: Slash,
                    lexeme: "/",
                    line: 1,
                },
                Token {
                    data: Bang,
                    lexeme: "!",
                    line: 1,
                },
                Token {
                    data: Less,
                    lexeme: "<",
                    line: 1,
                },
                Token {
                    data: Greater,
                    lexeme: ">",
                    line: 1,
                },
                eof(1),
            ]
        );
    }

    #[test]
    fn test_line_comment() {
        let source = "// this is a comment\n";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(tokens, vec![eof(2)]);
    }

    #[test]
    fn test_comments() {
        let source = "a // comment\nb";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Identifier,
                    lexeme: "a",
                    line: 1,
                },
                Token {
                    data: Identifier,
                    lexeme: "b",
                    line: 2,
                },
                eof(2),
            ]
        );
    }

    #[test]
    fn test_whitespace() {
        let source = " \t\r\n";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(tokens, vec![eof(2)]);
    }

    #[test]
    fn test_identifiers() {
        let source = "a abc _abc _abc123";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: Identifier,
                    lexeme: "a",
                    line: 1,
                },
                Token {
                    data: Identifier,
                    lexeme: "abc",
                    line: 1,
                },
                Token {
                    data: Identifier,
                    lexeme: "_abc",
                    line: 1,
                },
                Token {
                    data: Identifier,
                    lexeme: "_abc123",
                    line: 1,
                },
                eof(1),
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let source =
            "and class else false for fun if nil or print return super this true var while";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: And,
                    lexeme: "and",
                    line: 1,
                },
                Token {
                    data: Class,
                    lexeme: "class",
                    line: 1,
                },
                Token {
                    data: Else,
                    lexeme: "else",
                    line: 1,
                },
                Token {
                    data: False,
                    lexeme: "false",
                    line: 1,
                },
                Token {
                    data: For,
                    lexeme: "for",
                    line: 1,
                },
                Token {
                    data: Fun,
                    lexeme: "fun",
                    line: 1,
                },
                Token {
                    data: If,
                    lexeme: "if",
                    line: 1,
                },
                Token {
                    data: Nil,
                    lexeme: "nil",
                    line: 1,
                },
                Token {
                    data: Or,
                    lexeme: "or",
                    line: 1,
                },
                Token {
                    data: Print,
                    lexeme: "print",
                    line: 1,
                },
                Token {
                    data: Return,
                    lexeme: "return",
                    line: 1,
                },
                Token {
                    data: Super,
                    lexeme: "super",
                    line: 1,
                },
                Token {
                    data: This,
                    lexeme: "this",
                    line: 1,
                },
                Token {
                    data: True,
                    lexeme: "true",
                    line: 1,
                },
                Token {
                    data: Var,
                    lexeme: "var",
                    line: 1,
                },
                Token {
                    data: While,
                    lexeme: "while",
                    line: 1,
                },
                eof(1),
            ]
        );
    }
}

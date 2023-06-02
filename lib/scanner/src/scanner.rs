use std::{fmt::Display, todo, write, println};

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

mod token;
pub use token::Token;
use token::TokenData::{self, *};

pub struct Scanner {
    start: usize,
    current: usize,
    line: usize,
    source: Vec<char>,
    tokens: Vec<Token>,
    errors: Errors,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
            source: source.chars().collect(),
            tokens: Vec::new(),
            errors: Errors(Vec::new()),
        }
    }

    fn add_token(&mut self, data: TokenData) {
        self.tokens.push(Token {
            data,
            lexeme: self.source[self.start..self.current].iter().collect(),
            line: self.line,
        })
    }

    fn peek_next(&mut self) -> Option<char> {
        self.source.get(self.current + 1).copied()
    }

    fn peek(&mut self) -> Option<char> {
        self.source.get(self.current).copied()
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.source.get(self.current);
        self.current += 1;
        if c == Some(&'\n') {
            self.line += 1;
        }
        c.copied()
    }

    fn try_consume(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.consume();
            true
        } else {
            false
        }
    }

    fn consume_until_char(&mut self, stop: char) -> bool {
        loop {
            match self.consume() {
                Some(c) if c == stop => return true,
                Some('\n') => return false,
                None => return false,
                _ => (),
            };
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, Errors> {
        while let Some(c) = self.consume() {
            self.start = self.current - 1;
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
                        self.consume_until_char('\n');
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
        }
        self.tokens.push(Token {
            data: Eof,
            lexeme: "".to_string(),
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
                },
                Some('/') if self.peek() == Some('*') => {
                    self.consume();
                    nest_level += 1;
                }
                _ => (),
            }
        }
    }

    fn string(&mut self) {
        if !self.consume_until_char('"') {
            self.errors.0.push(Error::UnterminatedString(
                self.source[self.start + 1..self.current - 1]
                    .iter()
                    .collect(),
            ));
        } else {
            self.add_token(StringLiteral(String::from_iter(
                &self.source[self.start + 1..self.current - 1],
            )));
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

        self.add_token(Number(
            self.source[self.start..self.current]
                .iter()
                .collect::<String>()
                .parse()
                .unwrap(),
        ));
    }

    fn identifier(&mut self) {
        while self.peek().map(Self::is_alphanumeric_or_underscore).unwrap_or(false) {
            self.consume();
        }

        let text: String = self.source[self.start..self.current]
            .iter()
            .collect();

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

    fn eof(line: usize) -> Token {
        Token {
            data: Eof,
            lexeme: "".to_string(),
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
                    lexeme: "123".to_string(),
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
                    lexeme: "123".to_string(),
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
                    lexeme: "123".to_string(),
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
                    lexeme: "123".to_string(),
                    line: 1,
                },
                Token {
                    data: Number(1.23),
                    lexeme: "1.23".to_string(),
                    line: 1,
                },
                Token {
                    data: Number(1.0),
                    lexeme: "1".to_string(),
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
                    data: StringLiteral("hello world".to_string()),
                    lexeme: "\"hello world\"".to_string(),
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
            Errors(vec![Error::UnterminatedString("hello world".to_string())])
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
                    lexeme: "!".to_string(),
                    line: 1,
                },
                Token {
                    data: BangEqual,
                    lexeme: "!=".to_string(),
                    line: 1,
                },
                Token {
                    data: Equal,
                    lexeme: "=".to_string(),
                    line: 1,
                },
                Token {
                    data: EqualEqual,
                    lexeme: "==".to_string(),
                    line: 1,
                },
                Token {
                    data: Less,
                    lexeme: "<".to_string(),
                    line: 1,
                },
                Token {
                    data: LessEqual,
                    lexeme: "<=".to_string(),
                    line: 1,
                },
                Token {
                    data: Greater,
                    lexeme: ">".to_string(),
                    line: 1,
                },
                Token {
                    data: GreaterEqual,
                    lexeme: ">=".to_string(),
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
                    lexeme: "=".to_string(),
                    line: 1,
                },
                Token {
                    data: LeftParen,
                    lexeme: "(".to_string(),
                    line: 1,
                },
                Token {
                    data: RightParen,
                    lexeme: ")".to_string(),
                    line: 1,
                },
                Token {
                    data: LeftBrace,
                    lexeme: "{".to_string(),
                    line: 1,
                },
                Token {
                    data: RightBrace,
                    lexeme: "}".to_string(),
                    line: 1,
                },
                Token {
                    data: Comma,
                    lexeme: ",".to_string(),
                    line: 1,
                },
                Token {
                    data: Dot,
                    lexeme: ".".to_string(),
                    line: 1,
                },
                Token {
                    data: Minus,
                    lexeme: "-".to_string(),
                    line: 1,
                },
                Token {
                    data: Plus,
                    lexeme: "+".to_string(),
                    line: 1,
                },
                Token {
                    data: Semicolon,
                    lexeme: ";".to_string(),
                    line: 1,
                },
                Token {
                    data: Star,
                    lexeme: "*".to_string(),
                    line: 1,
                },
                Token {
                    data: Slash,
                    lexeme: "/".to_string(),
                    line: 1,
                },
                Token {
                    data: Bang,
                    lexeme: "!".to_string(),
                    line: 1,
                },
                Token {
                    data: Less,
                    lexeme: "<".to_string(),
                    line: 1,
                },
                Token {
                    data: Greater,
                    lexeme: ">".to_string(),
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
                    lexeme: "a".to_string(),
                    line: 1,
                },
                Token {
                    data: Identifier,
                    lexeme: "b".to_string(),
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
                    lexeme: "a".to_string(),
                    line: 1,
                },
                Token {
                    data: Identifier,
                    lexeme: "abc".to_string(),
                    line: 1,
                },
                Token {
                    data: Identifier,
                    lexeme: "_abc".to_string(),
                    line: 1,
                },
                Token {
                    data: Identifier,
                    lexeme: "_abc123".to_string(),
                    line: 1,
                },
                eof(1),
            ]
        );
    }

    #[test]
    fn test_keywords(){
        let source = "and class else false for fun if nil or print return super this true var while";
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    data: And,
                    lexeme: "and".to_string(),
                    line: 1,
                },
                Token {
                    data: Class,
                    lexeme: "class".to_string(),
                    line: 1,
                },
                Token {
                    data: Else,
                    lexeme: "else".to_string(),
                    line: 1,
                },
                Token {
                    data: False,
                    lexeme: "false".to_string(),
                    line: 1,
                },
                Token {
                    data: For,
                    lexeme: "for".to_string(),
                    line: 1,
                },
                Token {
                    data: Fun,
                    lexeme: "fun".to_string(),
                    line: 1,
                },
                Token {
                    data: If,
                    lexeme: "if".to_string(),
                    line: 1,
                },
                Token {
                    data: Nil,
                    lexeme: "nil".to_string(),
                    line: 1,
                },
                Token {
                    data: Or,
                    lexeme: "or".to_string(),
                    line: 1,
                },
                Token {
                    data: Print,
                    lexeme: "print".to_string(),
                    line: 1,
                },
                Token {
                    data: Return,
                    lexeme: "return".to_string(),
                    line: 1,
                },
                Token {
                    data: Super,
                    lexeme: "super".to_string(),
                    line: 1,
                },
                Token {
                    data: This,
                    lexeme: "this".to_string(),
                    line: 1,
                },
                Token {
                    data: True,
                    lexeme: "true".to_string(),
                    line: 1,
                },
                Token {
                    data: Var,
                    lexeme: "var".to_string(),
                    line: 1,
                },
                Token {
                    data: While,
                    lexeme: "while".to_string(),
                    line: 1,
                },
                eof(1),
            ]
        );
    }
}

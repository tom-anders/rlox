use std::{fmt::Display, todo, write};

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

    fn peek(&mut self) -> Option<&char> {
        self.source.get(self.current)
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.source.get(self.current);
        self.current += 1;
        c.copied()
    }

    fn consume_until_char(&mut self, stop: char) -> bool {
        self.consume_until(|c| c == stop)
    }

    fn consume_until(&mut self, stop: impl Fn(char) -> bool) -> bool {
        loop {
            match self.consume() {
                Some(c) if stop(c) => return true,
                Some('\n') => return false,
                None => return false,
                _ => (),
            };
        }
    }
    fn consume_if_matches(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(c) if *c == expected => {
                self.consume();
                true
            }
            _ => false,
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
                    if self.consume_if_matches('=') {
                        self.add_token(BangEqual)
                    } else {
                        self.add_token(Bang)
                    }
                }

                '=' => {
                    if self.consume_if_matches('=') {
                        self.add_token(EqualEqual)
                    } else {
                        self.add_token(Equal)
                    }
                }

                '<' => {
                    if self.consume_if_matches('=') {
                        self.add_token(LessEqual)
                    } else {
                        self.add_token(Less)
                    }
                }

                '>' => {
                    if self.consume_if_matches('=') {
                        self.add_token(GreaterEqual)
                    } else {
                        self.add_token(Greater)
                    }
                }

                '/' => {
                    if self.consume_if_matches('/') {
                        // Comment
                        self.consume_until_char('\n');
                    } else {
                        self.add_token(Slash)
                    }
                }

                d if d.is_ascii_digit() => self.number(),

                '"' => self.string(),

                ' ' | '\r' | '\t' => (),

                '\n' => self.line += 1,

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

    fn number(&mut self) {
        todo!("Implement peek_next() - probably want to ditch the iterator now?");
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
                eof(1),
            ]
        );
    }
}

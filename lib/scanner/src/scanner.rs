use std::{
    fmt::{Display, UpperExp},
    iter::{Enumerate, Peekable},
    println,
    str::Chars,
    todo, write,
};

use itertools::Itertools;

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacted(char),
    #[error("Unterminated string: {0}")]
    UnterminatedString(String),
}

#[derive(thiserror::Error, Debug)]
pub struct Errors(Vec<Error>);

impl Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|e| e.to_string()).join("\n"))
    }
}

mod token;
pub use token::Token;
use token::TokenData::{self, *};

pub struct Scanner<'a> {
    start: usize,
    current: usize,
    line: usize,
    source: &'a str,
    iter: Peekable<Enumerate<Chars<'a>>>,
    tokens: Vec<Token<'a>>,
    errors: Errors,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
            source,
            iter: source.chars().enumerate().peekable(),
            tokens: Vec::new(),
            errors: Errors(Vec::new()),
        }
    }

    fn add_token(&mut self, data: TokenData<'a>) {
        self.tokens.push(Token {
            data,
            lexeme: &self.source[self.start..self.current + 1],
            line: self.line,
        })
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|(_, c)| *c)
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let next = self.iter.next()?;
        self.current = next.0;
        Some(next)
    }

    fn advance_until_char(&mut self, stop: char) -> bool {
        self.advance_until(|c| c == stop)
    }
    fn advance_until(&mut self, stop: impl Fn(char) -> bool) -> bool {
        loop {
            match self.advance() {
                Some((_, c)) if stop(c) => return true,
                Some((_, '\n')) => return false,
                None => return false,
                _ => (),
            };
        }
    }
    fn advance_if_matches(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(c) if c == expected => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token<'a>>, Errors> {
        while let Some((start, c)) = self.advance() {
            self.start = start;
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
                    if self.advance_if_matches('=') {
                        self.add_token(BangEqual)
                    } else {
                        self.add_token(Bang)
                    }
                }

                '=' => {
                    if self.advance_if_matches('=') {
                        self.add_token(EqualEqual)
                    } else {
                        self.add_token(Equal)
                    }
                }

                '<' => {
                    if self.advance_if_matches('=') {
                        self.add_token(LessEqual)
                    } else {
                        self.add_token(Less)
                    }
                }

                '>' => {
                    if self.advance_if_matches('=') {
                        self.add_token(GreaterEqual)
                    } else {
                        self.add_token(Greater)
                    }
                }

                '/' => {
                    if self.advance_if_matches('/') {
                        // Comment
                        self.advance_until_char('\n');
                    } else {
                        self.add_token(Slash)
                    }
                }

                d if d.is_ascii_digit() => {
                    self.number()
                },

                '"' => self.string(),

                ' ' | '\r' | '\t' => (),

                '\n' => self.line += 1,

                c => self.errors.0.push(Error::UnexpectedCharacted(c)),
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

    fn string(&mut self) {
        if !self.advance_until_char('"') {
            self.errors.0.push(Error::UnterminatedString(
                self.source[self.start + 1..self.current].to_string(),
            ));
        } else {
            self.add_token(StringLiteral(&self.source[self.start + 1..self.current]));
        }
    }

    fn number(&mut self) {
        todo!("Implement peek_next() - probably want to ditch the iterator now?");
    }
}

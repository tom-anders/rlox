#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ScanError<'a> {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),
    #[error("Unterminated string: {0}")]
    UnterminatedString(&'a str),
    #[error("Unterminated block comment: {0}")]
    UnterminatedBlockComment(&'a str),
}

pub mod token;

use cursor::Cursor;
use errors::{Result, RloxError};
use token::TokenData::{self, *};
pub use token::*;

#[derive(Debug, Clone)]
struct Scanner<'a> {
    start: Cursor<'a>,
    cursor: Cursor<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let start = Cursor::new(source);
        let cursor = start.clone();
        Self { start, cursor }
    }

    fn peek(&self) -> Option<char> {
        self.cursor.peek()
    }

    fn consume(&mut self) -> Option<char> {
        self.cursor.next()
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

    fn token(&self, data: TokenData<'a>) -> Result<Token<'a>> {
        Ok(Token::new(data, (self.start.clone(), self.cursor.clone())))
    }

    fn error(&self, error: ScanError) -> Result<Token<'a>> {
        let prev = self.cursor.prev().unwrap();
        Err(RloxError { line: prev.line(), col: prev.col(), message: error.to_string() })
    }

    pub fn scan_token(&mut self) -> Option<Result<Token<'a>>> {
        self.start = self.cursor.clone();
        match self.consume()? {
            '(' => self.token(LeftParen),
            ')' => self.token(RightParen),
            '{' => self.token(LeftBrace),
            '}' => self.token(RightBrace),
            ',' => self.token(Comma),
            '.' => self.token(Dot),
            '-' => self.token(Minus),
            '+' => self.token(Plus),
            ';' => self.token(Semicolon),
            '*' => self.token(Star),

            '!' => {
                if self.try_consume('=') {
                    self.token(BangEqual)
                } else {
                    self.token(Bang)
                }
            }

            '=' => {
                if self.try_consume('=') {
                    self.token(EqualEqual)
                } else {
                    self.token(Equal)
                }
            }

            '<' => {
                if self.try_consume('=') {
                    self.token(LessEqual)
                } else {
                    self.token(Less)
                }
            }

            '>' => {
                if self.try_consume('=') {
                    self.token(GreaterEqual)
                } else {
                    self.token(Greater)
                }
            }

            '/' => {
                if self.try_consume('/') {
                    // Comment
                    self.consume_until('\n');
                    self.scan_token()?
                } else if self.try_consume('*') {
                    match self.block_comment() {
                        Ok(_) => self.scan_token()?,
                        Err(e) => Err(e),
                    }
                } else {
                    self.token(Slash)
                }
            }

            d if d.is_ascii_digit() => self.number(),

            '"' => self.string(),

            a if Self::is_alpha_or_underscore(a) => self.identifier(),

            ' ' | '\r' | '\t' | '\n' => self.scan_token()?,

            c => self.error(ScanError::UnexpectedCharacter(c)),
        }
        .into()
    }

    fn is_alpha_or_underscore(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_alphanumeric_or_underscore(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn block_comment(&mut self) -> Result<()> {
        let mut nest_level = 1;

        loop {
            match self.consume() {
                Some('*') if self.peek() == Some('/') => {
                    if nest_level == 1 {
                        self.consume();
                        return Ok(());
                    } else {
                        nest_level -= 1;
                    }
                }
                Some('/') if self.peek() == Some('*') => {
                    self.consume();
                    nest_level += 1;
                }
                None => {
                    return self
                        .error(ScanError::UnterminatedBlockComment(&self.lexeme()[2..]))
                        .map(|_| ())
                }
                _ => (),
            }
        }
    }

    fn lexeme(&self) -> &'a str {
        self.start.slice_until(&self.cursor)
    }

    fn string(&mut self) -> Result<Token<'a>> {
        if !self.consume_until('"') {
            self.error(ScanError::UnterminatedString(&self.lexeme()[1..]))
        } else {
            let s = self.lexeme();
            self.token(TokenData::Str(&s[1..s.len() - 1]))
        }
    }

    fn consume_digits(&mut self) {
        while self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            self.consume();
        }
    }

    fn number(&mut self) -> Result<Token<'a>> {
        self.consume_digits();

        if self.peek() == Some('.')
            && self.cursor.peek_next().map(|c| c.is_ascii_digit()) == Some(true)
        {
            self.consume();

            self.consume_digits();
        }

        self.token(Number(self.lexeme().parse().unwrap()))
    }

    fn identifier(&mut self) -> Result<Token<'a>> {
        while self.peek().map(Self::is_alphanumeric_or_underscore).unwrap_or(false) {
            self.consume();
        }

        match self.lexeme() {
            "and" => self.token(And),
            "class" => self.token(Class),
            "else" => self.token(Else),
            "false" => self.token(False),
            "for" => self.token(For),
            "fun" => self.token(Fun),
            "if" => self.token(If),
            "nil" => self.token(Nil),
            "or" => self.token(Or),
            "print" => self.token(Print),
            "return" => self.token(Return),
            "super" => self.token(Super),
            "this" => self.token(This),
            "true" => self.token(True),
            "var" => self.token(Var),
            "while" => self.token(While),
            _ => self.token(Identifier),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    scanner: Scanner<'a>,
    eof: bool,
}

impl<'a> TokenStream<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { scanner: Scanner::new(source), eof: false }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.scanner.scan_token();

        match next {
            None if !self.eof => {
                self.eof = true;
                Some(Ok(Token::new(
                    Eof,
                    (self.scanner.cursor.clone(), self.scanner.cursor.clone()),
                )))
            }
            _ => next,
        }
    }
}

#[cfg(test)]
mod tests {
    use cursor::{Col, Line};
    use pretty_assertions::assert_eq;

    use errors::RloxError;

    use super::*;

    #[derive(Debug, PartialEq)]
    struct ExpectedToken<'a> {
        data: TokenData<'a>,
        line: Line,
        col: Col,
        lexeme: String,
    }

    impl<'a> From<Token<'a>> for ExpectedToken<'a> {
        fn from(token: Token<'a>) -> Self {
            ExpectedToken {
                data: token.data.clone(),
                line: token.line(),
                col: token.col(),
                lexeme: token.lexeme().to_string(),
            }
        }
    }

    fn scan_expected_tokens(source: &str) -> Vec<Result<ExpectedToken>> {
        TokenStream::new(source).map(|t| t.map(ExpectedToken::from)).collect()
    }

    #[test]
    fn unexpected_char() {
        assert_eq!(
            scan_expected_tokens("var x = 3;\n  @"),
            vec![
                Ok(ExpectedToken {
                    data: Var,
                    line: Line(1),
                    col: Col(1),
                    lexeme: "var".to_string()
                }),
                Ok(ExpectedToken {
                    data: Identifier,
                    line: Line(1),
                    col: Col(5),
                    lexeme: "x".to_string()
                }),
                Ok(ExpectedToken {
                    data: Equal,
                    line: Line(1),
                    col: Col(7),
                    lexeme: "=".to_string()
                }),
                Ok(ExpectedToken {
                    data: Number(3.0),
                    line: Line(1),
                    col: Col(9),
                    lexeme: "3".to_string()
                }),
                Ok(ExpectedToken {
                    data: Semicolon,
                    line: Line(1),
                    col: Col(10),
                    lexeme: ";".to_string()
                }),
                Err(RloxError {
                    line: Line(2),
                    col: Col(3),
                    message: ScanError::UnexpectedCharacter('@').to_string()
                }),
                Ok(ExpectedToken { data: Eof, line: Line(2), col: Col(4), lexeme: "".to_string() })
            ]
        );
    }

    #[test]
    fn unterminated_block_comment() {
        assert_eq!(
            scan_expected_tokens("/* hello world"),
            vec![
                Err(RloxError {
                    line: Line(1),
                    col: Col(14),
                    message: ScanError::UnterminatedBlockComment(" hello world").to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(1),
                    col: Col(15),
                    lexeme: "".to_string()
                })
            ]
        );

        assert_eq!(
            scan_expected_tokens("/* hello\nworld"),
            vec![
                Err(RloxError {
                    line: Line(2),
                    col: Col(5),
                    message: ScanError::UnterminatedBlockComment(" hello\nworld").to_string()
                }),
                Ok(ExpectedToken { data: Eof, line: Line(2), col: Col(6), lexeme: "".to_string() })
            ]
        );
    }

    #[test]
    fn test_block_comments() {
        assert_eq!(
            scan_expected_tokens("/* hello world */"),
            vec![Ok(ExpectedToken {
                data: Eof,
                line: Line(1),
                col: Col(18),
                lexeme: "".to_string()
            })]
        );

        assert_eq!(
            scan_expected_tokens("/* hello world */ 123"),
            vec![
                Ok(ExpectedToken {
                    data: Number(123.0),
                    line: Line(1),
                    col: Col(19),
                    lexeme: "123".to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(1),
                    col: Col(22),
                    lexeme: "".to_string()
                })
            ]
        );

        assert_eq!(
            scan_expected_tokens("/* hello world \n */ 123"),
            vec![
                Ok(ExpectedToken {
                    data: Number(123.0),
                    line: Line(2),
                    col: Col(5),
                    lexeme: "123".to_string()
                }),
                Ok(ExpectedToken { data: Eof, line: Line(2), col: Col(8), lexeme: "".to_string() })
            ]
        );

        // test nested block comments
        assert_eq!(
            scan_expected_tokens("/* hello /* world */ */ 123 /* trailing block comment */"),
            vec![
                Ok(ExpectedToken {
                    data: Number(123.0),
                    line: Line(1),
                    col: Col(25),
                    lexeme: "123".to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(1),
                    col: Col(57),
                    lexeme: "".to_string()
                })
            ]
        );
    }

    #[test]
    fn test_number_literals() {
        assert_eq!(
            scan_expected_tokens("123 1.23 1"),
            vec![
                Ok(ExpectedToken {
                    data: Number(123.0),
                    line: Line(1),
                    col: Col(1),
                    lexeme: "123".to_string()
                }),
                Ok(ExpectedToken {
                    data: Number(1.23),
                    line: Line(1),
                    col: Col(5),
                    lexeme: "1.23".to_string()
                }),
                Ok(ExpectedToken {
                    data: Number(1.0),
                    line: Line(1),
                    col: Col(10),
                    lexeme: "1".to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(1),
                    col: Col(11),
                    lexeme: "".to_string()
                })
            ]
        );
    }

    #[test]
    fn string_literals() {
        assert_eq!(
            scan_expected_tokens("\"hello world\""),
            vec![
                Ok(ExpectedToken {
                    data: Str("hello world"),
                    line: Line(1),
                    col: Col(1),
                    lexeme: "\"hello world\"".to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(1),
                    col: Col(14),
                    lexeme: "".to_string()
                })
            ]
        );

        assert_eq!(
            scan_expected_tokens("     \n\n \"hello world"),
            vec![
                Err(RloxError {
                    line: Line(3),
                    col: Col(13),
                    message: ScanError::UnterminatedString("hello world").to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(3),
                    col: Col(14),
                    lexeme: "".to_string()
                })
            ]
        );

        assert_eq!(
            scan_expected_tokens("\"hello world\n"),
            vec![
                Err(RloxError {
                    line: Line(1),
                    col: Col(13),
                    message: ScanError::UnterminatedString("hello world\n").to_string()
                }),
                Ok(ExpectedToken { data: Eof, line: Line(2), col: Col(1), lexeme: "".to_string() })
            ]
        );
    }
    #[test]
    fn two_char_tokens() {
        assert_eq!(
            scan_expected_tokens("! != = == < <= > >="),
            vec![
                Ok(ExpectedToken {
                    data: TokenData::Bang,
                    line: Line(1),
                    col: Col(1),
                    lexeme: "!".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::BangEqual,
                    line: Line(1),
                    col: Col(3),
                    lexeme: "!=".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Equal,
                    line: Line(1),
                    col: Col(6),
                    lexeme: "=".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::EqualEqual,
                    line: Line(1),
                    col: Col(8),
                    lexeme: "==".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Less,
                    line: Line(1),
                    col: Col(11),
                    lexeme: "<".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::LessEqual,
                    line: Line(1),
                    col: Col(13),
                    lexeme: "<=".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Greater,
                    line: Line(1),
                    col: Col(16),
                    lexeme: ">".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::GreaterEqual,
                    line: Line(1),
                    col: Col(18),
                    lexeme: ">=".to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(1),
                    col: Col(20),
                    lexeme: "".to_string()
                })
            ]
        );
    }

    #[test]
    fn test_line_comment() {
        assert_eq!(
            scan_expected_tokens("// hello world"),
            vec![Ok(ExpectedToken {
                data: Eof,
                line: Line(1),
                col: Col(15),
                lexeme: "".to_string()
            })]
        );

        assert_eq!(
            scan_expected_tokens("// hello world\n123"),
            vec![
                Ok(ExpectedToken {
                    data: Number(123.0),
                    line: Line(2),
                    col: Col(1),
                    lexeme: "123".to_string()
                }),
                Ok(ExpectedToken { data: Eof, line: Line(2), col: Col(4), lexeme: "".to_string() })
            ]
        );
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(
            scan_expected_tokens(" \t\r\n"),
            vec![Ok(ExpectedToken {
                data: Eof,
                line: Line(2),
                col: Col(1),
                lexeme: "".to_string()
            })]
        );
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(
            scan_expected_tokens("a a_c _abc _abc123"),
            vec![
                Ok(ExpectedToken {
                    data: Identifier,
                    line: Line(1),
                    col: Col(1),
                    lexeme: "a".to_string()
                }),
                Ok(ExpectedToken {
                    data: Identifier,
                    line: Line(1),
                    col: Col(3),
                    lexeme: "a_c".to_string()
                }),
                Ok(ExpectedToken {
                    data: Identifier,
                    line: Line(1),
                    col: Col(7),
                    lexeme: "_abc".to_string()
                }),
                Ok(ExpectedToken {
                    data: Identifier,
                    line: Line(1),
                    col: Col(12),
                    lexeme: "_abc123".to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(1),
                    col: Col(19),
                    lexeme: "".to_string()
                })
            ]
        );
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            scan_expected_tokens(
                "and class else false for fun if nil or print return super this true var while"
            ),
            vec![
                Ok(ExpectedToken {
                    data: TokenData::And,
                    line: Line(1),
                    col: Col(1),
                    lexeme: "and".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Class,
                    line: Line(1),
                    col: Col(5),
                    lexeme: "class".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Else,
                    line: Line(1),
                    col: Col(11),
                    lexeme: "else".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::False,
                    line: Line(1),
                    col: Col(16),
                    lexeme: "false".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::For,
                    line: Line(1),
                    col: Col(22),
                    lexeme: "for".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Fun,
                    line: Line(1),
                    col: Col(26),
                    lexeme: "fun".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::If,
                    line: Line(1),
                    col: Col(30),
                    lexeme: "if".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Nil,
                    line: Line(1),
                    col: Col(33),
                    lexeme: "nil".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Or,
                    line: Line(1),
                    col: Col(37),
                    lexeme: "or".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Print,
                    line: Line(1),
                    col: Col(40),
                    lexeme: "print".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Return,
                    line: Line(1),
                    col: Col(46),
                    lexeme: "return".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Super,
                    line: Line(1),
                    col: Col(53),
                    lexeme: "super".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::This,
                    line: Line(1),
                    col: Col(59),
                    lexeme: "this".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::True,
                    line: Line(1),
                    col: Col(64),
                    lexeme: "true".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::Var,
                    line: Line(1),
                    col: Col(69),
                    lexeme: "var".to_string()
                }),
                Ok(ExpectedToken {
                    data: TokenData::While,
                    line: Line(1),
                    col: Col(73),
                    lexeme: "while".to_string()
                }),
                Ok(ExpectedToken {
                    data: Eof,
                    line: Line(1),
                    col: Col(78),
                    lexeme: "".to_string()
                })
            ]
        );
    }
}

use std::{str::Chars};

// TODO source -> cursor

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    source: &'a str,
    chars: Chars<'a>,
}

// impl PartialEq for Cursor
impl <'a> PartialEq for Cursor<'a> {
    fn eq(&self, other: &Self) -> bool {
        (self.source, self.chars.as_str()) == (other.source, other.chars.as_str())
    }
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, chars: source.chars(), }
    }

    pub fn line(&self) -> usize {
        let mut line = 1;
        let mut chars = self.source.chars();

        while chars.as_str() != self.chars.as_str() {
            if chars.next() == Some('\n') {
                line += 1;
            }
        }

        line
    }

    pub fn col(&self) -> usize {
        let mut col = 1;
        let mut chars = self.source.chars();

        while chars.as_str() != self.chars.as_str() {
            if chars.next() == Some('\n') {
                col = 1;
            } else {
                col += 1;
            }
        }

        col
    }
}

impl<'a> From<&'a str> for Cursor<'a> {
    fn from(source: &'a str) -> Self {
        Self::new(source)
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }
}

impl<'a> Cursor<'a> {
    pub fn slice_until<'c>(&self, end: &'c Cursor<'a>) -> &'a str {
        assert!(self.source == end.source);
        &self.source[(self.source.len() - self.chars.as_str().len())
            ..(self.source.len() - end.chars.as_str().len())]
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn peek_next(&self) -> Option<char> {
        self.chars.clone().nth(1)
    }

    // Watch out, this is O(n)! Should only be used for error reporting.
    pub fn prev(&self) -> Option<Cursor> {
        if self.chars.as_str() == self.source {
            return None;
        }

        let chars = self.source[self.source.len() - self.chars.as_str().len() - 1..].chars();

        Some(Cursor {
            source: self.source,
            chars,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;

    #[test]
    fn slice_until() {
        let mut cursor: Cursor = "ab\ncd\n\n".into();

        cursor.next(); // 'a'

        let start = cursor.clone();

        cursor.next(); // 'b'
        cursor.next(); // '\n'
        cursor.next(); // 'c'

        assert_eq!(start.slice_until(&cursor), "b\nc");
    }

    #[test]
    fn test_next_and_prev() {
        let source = "ab\ncd\n\n";
        let mut cursor = Cursor::new(source);

        assert_eq!(cursor.peek(), Some('a'));
        assert_eq!(cursor.peek_next(), Some('b'));
        assert_eq!(cursor.next(), Some('a'));

        assert_eq!((cursor.line(), cursor.col()), (1, 2));

        let mut prev = cursor.prev().unwrap();
        assert_eq!((prev.line(), prev.col()), (1, 1));
        assert_eq!(prev.next(), Some('a'));

        assert_eq!(cursor.next(), Some('b'));
        assert_eq!((cursor.line(), cursor.col()), (1, 3));

        assert_eq!(cursor.next(), Some('\n'));
        assert_eq!((cursor.line(), cursor.col()), (2, 1));

        let mut prev = cursor.prev().unwrap();
        assert_eq!((prev.line(), prev.col()), (1, 3));
        assert_eq!(prev.next(), Some('\n'));

        cursor.next(); // 'c'
        cursor.next(); // 'd'

        assert_eq!(cursor.next(), Some('\n'));
        assert_eq!((cursor.line(), cursor.col()), (3, 1));

        assert_eq!(cursor.next(), Some('\n'));
        assert_eq!((cursor.line(), cursor.col()), (4, 1));

        let mut prev = cursor.prev().unwrap();
        assert_eq!((prev.line(), prev.col()), (3, 1));
        assert_eq!(prev.next(), Some('\n'));

        assert_eq!(cursor.peek(), None);
        assert_eq!(cursor.next(), None);
        assert_eq!((cursor.line(), cursor.col()), (4, 1));

        let mut prev = cursor.prev().unwrap();
        assert_eq!((prev.line(), prev.col()), (3, 1));
        assert_eq!(prev.next(), Some('\n'));

        cursor = "".into();
        assert_eq!(cursor.peek(), None);
        assert_eq!(cursor.peek_next(), None);
        assert_eq!(cursor.next(), None);
        assert_eq!((cursor.line(), cursor.col()), (1, 1));

        cursor = "a".into();
        assert_eq!(cursor.peek(), Some('a'));
        assert_eq!(cursor.next(), Some('a'));
        assert_eq!((cursor.line(), cursor.col()), (1, 2));

        let mut prev = cursor.prev().unwrap();
        assert_eq!((prev.line(), prev.col()), (1, 1));
        assert_eq!(prev.next(), Some('a'));

        cursor = "\n".into();
        assert_eq!(cursor.peek(), Some('\n'));
        assert_eq!(cursor.peek_next(), None);
        assert_eq!(cursor.next(), Some('\n'));
        assert_eq!((cursor.line(), cursor.col()), (2, 1));

        cursor = "\n\n".into();
        assert_eq!(cursor.next(), Some('\n'));
        assert_eq!((cursor.line(), cursor.col()), (2, 1));
        assert_eq!(cursor.next(), Some('\n'));
        assert_eq!((cursor.line(), cursor.col()), (3, 1));

        assert_eq!(cursor.prev().unwrap().next(), Some('\n'));
        assert_eq!(cursor.prev().unwrap().prev().unwrap().next(), Some('\n'));
        assert_eq!(cursor.prev().unwrap().prev().unwrap().prev(), None);
    }
}

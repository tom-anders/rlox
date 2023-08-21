use crate::{Col, Cursor, Line};

#[derive(Clone, PartialEq)]
pub struct SourceRange<'a> {
    start: Cursor<'a>,
    end: Cursor<'a>,
}

impl<'a> std::fmt::Debug for SourceRange<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.debug_struct("SourceRange")
                .field("start", &self.start)
                .field("end", &self.end)
                .finish()
        } else {
            write!(f, "\"{}\"", self.lexeme())
        }
    }
}

impl<'a> From<(Cursor<'a>, Cursor<'a>)> for SourceRange<'a> {
    fn from((start, end): (Cursor<'a>, Cursor<'a>)) -> Self {
        Self::new(start, end)
    }
}

impl<'a> SourceRange<'a> {
    pub fn new(start: Cursor<'a>, end: Cursor<'a>) -> Self {
        assert!(start.chars.as_str().len() >= end.chars.as_str().len());
        Self { start, end }
    }

    pub fn lexeme(&self) -> &'a str {
        self.start.slice_until(&self.end)
    }

    pub fn start(&self) -> &Cursor<'a> {
        &self.start
    }

    pub fn end(&self) -> &Cursor<'a> {
        &self.end
    }

    pub fn source(&self) -> &'a str {
        self.start.source
    }

    pub fn line(&self) -> Line {
        self.start.line()
    }

    pub fn col(&self) -> Col {
        self.start.col()
    }
}

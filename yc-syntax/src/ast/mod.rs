use core::fmt;
use std::{cmp::Ordering, error, fmt::Display, ops::Range};

mod char_stream;
mod lex;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Error {
    pub message: String,
    pub source_code: String,
    pub span: Span,
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{}: {}",
            self.span.start.line, self.span.start.col, self.message
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn range(&self) -> Range<usize> {
        self.start.offset..self.end.offset
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Span) -> Ordering {
        (&self.start, &self.end).cmp(&(&other.start, &other.end))
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Range<usize> {
        span.range()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
}

impl Position {
    pub fn new(offset: usize, line: usize, col: usize) -> Self {
        Self { offset, line, col }
    }

    pub fn advance_by(&self, c: char) -> Position {
        let mut advanced = *self;
        advanced.offset += c.len_utf8();
        if c == '\n' {
            advanced.line += 1;
            advanced.col = 1;
        } else {
            advanced.col += 1;
        }
        advanced
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            offset: 0,
            line: 1,
            col: 1,
        }
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Position) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Position) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

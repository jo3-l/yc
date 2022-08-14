use std::cmp::Ordering;
use std::ops::Range;

trait Spanned {
    fn span(self) -> Span;
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

    pub fn with_start(&self, start: Position) -> Self {
        Self {
            start,
            end: self.end,
        }
    }

    pub fn with_end(&self, end: Position) -> Self {
        Self {
            start: self.start,
            end,
        }
    }

    pub fn to(&self, span: Span) -> Self {
        Self {
            start: self.start,
            end: span.end,
        }
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
        let mut next = *self;
        next.offset += c.len_utf8();
        if c == '\n' {
            next.line += 1;
            next.col = 1;
        } else {
            next.col += 1;
        }
        next
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

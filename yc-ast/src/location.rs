use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Range, Sub, SubAssign};

/// A span of text within source code. The span is inclusive on the left end and
/// exclusive on the right.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Span {
    pub start: BytePos,
    pub end: BytePos,
}

impl Span {
    pub fn new(start: BytePos, end: BytePos) -> Self {
        Self { start, end }
    }

    pub fn as_range(&self) -> Range<usize> {
        self.start.pos..self.end.pos
    }

    /// Returns a new span from the specified start position to the end of this
    /// span.
    pub fn with_start(&self, start: BytePos) -> Self {
        Self {
            start,
            end: self.end,
        }
    }

    /// Returns a new span from the start of this span to the specified end
    /// position.
    pub fn with_end(&self, end: BytePos) -> Self {
        Self {
            start: self.start,
            end,
        }
    }

    /// Returns a new span from the start of this span to the end of the other
    /// span.
    pub fn to(&self, other: Span) -> Self {
        Self {
            start: self.start,
            end: other.end,
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
        span.as_range()
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Span {
        Span::new(range.start.into(), range.end.into())
    }
}

/// A byte offset into source code.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct BytePos {
    pos: usize,
}

impl BytePos {
    pub fn from_usize(pos: usize) -> Self {
        Self { pos }
    }

    pub fn as_usize(&self) -> usize {
        self.pos
    }
}

impl Add<usize> for BytePos {
    type Output = BytePos;

    fn add(self, offset: usize) -> Self::Output {
        Self::from_usize(self.as_usize() + offset)
    }
}

impl AddAssign<usize> for BytePos {
    fn add_assign(&mut self, offset: usize) {
        self.pos += offset;
    }
}

impl Sub<usize> for BytePos {
    type Output = BytePos;

    fn sub(self, offset: usize) -> Self::Output {
        Self::from_usize(self.as_usize() - offset)
    }
}

impl SubAssign<usize> for BytePos {
    fn sub_assign(&mut self, offset: usize) {
        self.pos -= offset;
    }
}

impl From<usize> for BytePos {
    fn from(pos: usize) -> BytePos {
        BytePos::from_usize(pos)
    }
}

impl From<BytePos> for usize {
    fn from(pos: BytePos) -> usize {
        pos.as_usize()
    }
}

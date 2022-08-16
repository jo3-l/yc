use yc_ast::location::{BytePos, Span};

/// A character stream used by the lexer that keeps track of the current
/// position within the source code.
pub(crate) struct CharStream<'a> {
    pub(crate) src: &'a str,
    pub(crate) pos: BytePos,
}

impl<'a> CharStream<'a> {
    pub(crate) fn new(src: &'a str) -> Self {
        Self {
            src,
            pos: BytePos::from_usize(0),
        }
    }

    /// Advances the stream if the pattern matches at the current position.
    ///
    /// Panics if the pattern does not match.
    pub(crate) fn must_consume(&mut self, pat: impl Pattern<'a>) {
        assert!(self.lookahead(pat));
        self.advance(pat.match_len());
    }

    /// If the pattern matches at the current position, advances the stream and
    /// returns true; otherwise, returns false.
    pub(crate) fn accept(&mut self, pat: impl Pattern<'a>) -> bool {
        if self.lookahead(pat) {
            self.advance(pat.match_len());
            true
        } else {
            false
        }
    }

    /// Indicates whether the pattern matches at the current position.
    pub(crate) fn lookahead<P>(&self, pat: P) -> bool
    where
        P: Pattern<'a>,
    {
        pat.is_prefix_of(self.remaining())
    }

    /// Advances the stream while the predicate matches the current character and
    /// returns the consumed text.
    pub(crate) fn consume_while<P>(&mut self, mut predicate: P) -> String
    where
        P: FnMut(char) -> bool,
    {
        let mut consumed = String::new();
        while let Some(c) = self.peek().filter(|c| predicate(*c)) {
            consumed.push(c);
            self.advance(1);
        }
        consumed
    }

    /// Advances the stream until the pattern matches or there are no more
    /// characters (whichever happens first) and returns the consumed text.
    pub(crate) fn consume_until<P>(&mut self, pattern: P) -> String
    where
        P: Pattern<'a>,
    {
        let mut consumed = String::new();
        while !self.at_eof() && !self.lookahead(pattern) {
            consumed.push(self.next().unwrap());
        }
        consumed
    }

    /// Consumes and returns the next character in the stream.
    pub(crate) fn next(&mut self) -> Option<char> {
        let c = self.remaining().chars().next()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    /// Consumes the next character in the stream and returns it along with its
    /// span.
    pub(crate) fn next_with_span(&mut self) -> Option<(char, Span)> {
        let c = self.remaining().chars().next()?;
        let start_pos = self.pos;
        self.pos += c.len_utf8();
        Some((c, self.span_from(start_pos)))
    }

    /// Retrieves but does not consume the next character in the stream.
    pub(crate) fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    /// Retrieves but does not consume the `n`th character starting from the
    /// current position, where `n` starts at 0.
    pub(crate) fn peek_nth(&self, n: usize) -> Option<char> {
        self.remaining().chars().nth(n)
    }

    /// Returns the span associated with the current character. If the end of
    /// the text has been reached, an empty span pointing to the end of the text
    /// will be returned.
    pub(crate) fn cur_span(&self) -> Span {
        Span::new(
            self.pos,
            self.pos + self.peek().map(|c| c.len_utf8()).unwrap_or_default(),
        )
    }

    /// Returns the span associated with the previous character read.
    pub(crate) fn prev_span(&self) -> Option<Span> {
        let prev_start = self.pos
            - self.src[..self.pos.as_usize()]
                .chars()
                .next_back()
                .map(|c| c.len_utf8())?;
        Some(Span::new(prev_start, self.pos))
    }

    /// Returns the span from the given position to the current position.
    pub(crate) fn span_from(&self, pos: BytePos) -> Span {
        Span::new(pos, self.pos)
    }

    /// Advances the character stream until `n` characters have been read or the
    /// end of the text is reached, whichever happens first.
    pub(crate) fn advance(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    /// Returns the remaining text yet to be consumed.
    pub(crate) fn remaining(&self) -> &'a str {
        &self.src[self.pos.as_usize()..]
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.pos.as_usize() >= self.src.len()
    }
}

/// A string pattern. Similar to [std::str::pattern::Pattern], but usable on
/// stable Rust.
pub(crate) trait Pattern<'a>: Sized + Copy {
    /// Indicates whether the pattern matches at the start of the haystack.
    fn is_prefix_of(self, haystack: &'a str) -> bool;

    /// Returns the number of characters matched by this pattern.
    fn match_len(self) -> usize;
}

impl<'a, 'b> Pattern<'a> for &'a str {
    fn is_prefix_of(self, haystack: &'a str) -> bool {
        haystack.starts_with(self)
    }

    fn match_len(self) -> usize {
        self.chars().count()
    }
}

impl<'a> Pattern<'a> for char {
    fn is_prefix_of(self, haystack: &'a str) -> bool {
        haystack.starts_with(self)
    }

    fn match_len(self) -> usize {
        1
    }
}

impl<'a, const N: usize> Pattern<'a> for &'a [char; N] {
    fn is_prefix_of(self, haystack: &'a str) -> bool {
        haystack.starts_with(self)
    }

    fn match_len(self) -> usize {
        1
    }
}

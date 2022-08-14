use yc_ast::location::{Position, Span};

pub(crate) struct CharStream<'a> {
    pub(crate) text: &'a str,
    pub(crate) pos: Position,
}

impl<'a> CharStream<'a> {
    pub(crate) fn new(text: &'a str) -> Self {
        Self {
            text,
            pos: Position::default(),
        }
    }

    pub(crate) fn must_consume<P>(&mut self, pat: P) -> Span
    where
        P: Pattern<'a>,
    {
        assert!(self.lookahead(pat));
        let start_pos = self.pos;
        self.skip(pat.match_len());
        self.span_after(start_pos)
    }

    pub(crate) fn accept<P>(&mut self, pat: P) -> bool
    where
        P: Pattern<'a>,
    {
        if self.lookahead(pat) {
            self.skip(pat.match_len());
            true
        } else {
            false
        }
    }

    pub(crate) fn lookahead<P>(&self, pat: P) -> bool
    where
        P: Pattern<'a>,
    {
        pat.is_prefix_of(self.remaining())
    }

    pub(crate) fn consume_while<P>(&mut self, mut predicate: P) -> String
    where
        P: FnMut(char) -> bool,
    {
        let mut consumed = String::new();
        while let Some(c) = self.peek().filter(|c| predicate(*c)) {
            consumed.push(c);
            self.skip(1);
        }
        consumed
    }

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

    pub(crate) fn next(&mut self) -> Option<char> {
        let c = self.remaining().chars().next()?;
        self.pos = self.pos.advance_by(c);
        Some(c)
    }

    pub(crate) fn next_with_span(&mut self) -> Option<(char, Span)> {
        let c = self.remaining().chars().next()?;
        let start_pos = self.pos;
        self.pos = self.pos.advance_by(c);
        Some((c, self.span_after(start_pos)))
    }

    pub(crate) fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    pub(crate) fn peek_nth(&self, n: usize) -> Option<char> {
        self.remaining().chars().nth(n)
    }

    pub(crate) fn span(&self) -> Span {
        let next_pos = if let Some(c) = self.peek() {
            self.pos.advance_by(c)
        } else {
            Position::new(self.text.len(), self.pos.line, self.pos.col + 1)
        };
        Span::new(self.pos, next_pos)
    }

    pub(crate) fn zero_width_span(&self) -> Span {
        Span::new(self.pos, self.pos)
    }

    pub(crate) fn span_after(&self, pos: Position) -> Span {
        Span::new(pos, self.pos)
    }

    pub(crate) fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    pub(crate) fn remaining(&self) -> &'a str {
        &self.text[self.offset()..]
    }

    pub(crate) fn offset(&self) -> usize {
        self.pos.offset
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.offset() >= self.text.len()
    }
}

pub(crate) trait Pattern<'a>: Sized + Copy {
    fn is_prefix_of(self, haystack: &'a str) -> bool;
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

impl<'a, 'b, const N: usize> Pattern<'a> for &'b [char; N] {
    fn is_prefix_of(self, haystack: &'a str) -> bool {
        haystack.starts_with(self)
    }

    fn match_len(self) -> usize {
        1
    }
}

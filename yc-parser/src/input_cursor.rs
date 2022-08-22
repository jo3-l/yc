use yc_ast::location::{BytePos, Span};

pub(crate) struct InputCursor<'src> {
    pub(crate) src: &'src str,
    pub(crate) pos: BytePos,
}

impl<'src> InputCursor<'src> {
    pub(crate) fn new(src: &'src str) -> Self {
        Self {
            src,
            pos: BytePos::from_usize(0),
        }
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.pos.as_usize() >= self.src.len()
    }

    /// Returns the text in front of the current position.
    pub(crate) fn remaining(&self) -> &'src str {
        &self.src[self.pos.as_usize()..]
    }

    /// Returns the character at the current position.
    pub(crate) fn first(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    /// Returns the span from the given position to the current position.
    pub(crate) fn span_after(&self, pos: BytePos) -> Span {
        Span::new(pos, self.pos)
    }

    /// Returns the span associated with the current character. If the end of
    /// the text has been reached, an empty span pointing to the end of the text
    /// will be returned.
    pub(crate) fn span(&self) -> Span {
        Span::new(
            self.pos,
            self.pos + self.first().map_or(0, |c| c.len_utf8()),
        )
    }

    /// Retrieves but does not consume the `n`th character starting from the
    /// current position, where `n` starts at 0.
    pub(crate) fn nth(&self, n: usize) -> Option<char> {
        self.remaining().chars().nth(n)
    }

    /// Advances the position of the cursor by one character.
    pub(crate) fn bump(&mut self) {
        self.pos += self.first().map_or(0, |c| c.len_utf8())
    }

    /// Advances the position of the cursor by `n` characters.
    pub(crate) fn bump_n(&mut self, n: usize) {
        for _ in 0..n {
            self.bump();
        }
    }

    /// Advances the position of the cursor as long as the predicate returns
    /// true for the current character.
    pub(crate) fn bump_while<P>(&mut self, mut predicate: P)
    where
        P: FnMut(char) -> bool,
    {
        while !self.at_eof() && self.first().filter(|c| predicate(*c)).is_some() {
            self.bump();
        }
    }
}

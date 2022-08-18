use std::collections::VecDeque;

use bitflags::bitflags;
use yc_ast::token::{Token, TokenKind};

use crate::lex::Lexer;

/// A source of tokens used by the parser that supports infinite lookahead.
pub(crate) struct TokenSource<'src> {
    lexer: Lexer<'src>,
    lookahead_buf: VecDeque<Token>,
}

impl<'src> TokenSource<'src> {
    pub(crate) fn new(lexer: Lexer<'src>) -> Self {
        Self {
            lexer,
            lookahead_buf: VecDeque::new(),
        }
    }

    /// Returns the next token that passes the filter.
    pub(crate) fn next(&mut self, filter: TokenFilter) -> Token {
        while let Some(token) = self.lookahead_buf.pop_front() {
            if !filter.should_skip(&token) {
                return token;
            }
        }

        loop {
            let token = self.lexer.next_token();
            if !filter.should_skip(&token) {
                return token;
            }
        }
    }

    /// Retrieves but does not consume the next token that passes the filter.
    pub(crate) fn peek(&mut self, filter: TokenFilter) -> &Token {
        self.peek_nth(0, filter)
    }

    /// Retrieves but does not consume the `n`th token that passes the filter
    /// starting from the current token, where `n` starts at `0`.
    pub(crate) fn peek_nth(&mut self, n: usize, filter: TokenFilter) -> &Token {
        if let Some(lookahead) = n.checked_sub(self.lookahead_buf.len()) {
            self.lookahead_buf.reserve(n - self.lookahead_buf.len() + 1);
            while self.lookahead_buf.len() <= n {
                let token = self.lexer.next_token();
                if !filter.should_skip(&token) {
                    self.lookahead_buf.push_back(token);
                }
            }
        }
        &self.lookahead_buf[n]
    }

    pub(crate) fn source(&self) -> &'src str {
        self.lexer.source()
    }
}

bitflags! {
    /// Specifies which kinds of tokens should be skipped, if any.
    #[derive(Default)]
    pub(crate) struct TokenFilter: u8 {
        const SKIP_INVALID = 1 << 0;
        const SKIP_WHITESPACE = 1 << 1;
    }
}

impl TokenFilter {
    fn none() -> Self {
        Self::empty()
    }

    fn should_skip(&self, token: &Token) -> bool {
        match token.kind {
            TokenKind::Invalid => self.contains(TokenFilter::SKIP_INVALID),
            TokenKind::Whitespace(_) => self.contains(TokenFilter::SKIP_WHITESPACE),
            _ => false,
        }
    }
}

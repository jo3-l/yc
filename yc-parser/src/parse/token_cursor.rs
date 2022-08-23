use std::collections::VecDeque;

use yc_ast::token::{Token, TokenKind};

use crate::lex::Lexer;

pub(crate) struct TokenCursor<'src> {
    lexer: Lexer<'src>,
    lookahead: VecDeque<Token>,
}

impl<'src> TokenCursor<'src> {
    pub(crate) fn new(lexer: Lexer<'src>) -> Self {
        Self {
            lexer,
            lookahead: VecDeque::new(),
        }
    }

    pub(crate) fn source(&self) -> &'src str {
        self.lexer.source()
    }

    pub(crate) fn first(&mut self) -> Token {
        self.nth(0)
    }

    pub(crate) fn first_non_space(&mut self) -> Token {
        self.nth_non_space(0)
    }

    pub(crate) fn nth(&mut self, n: usize) -> Token {
        if n >= self.lookahead.len() {
            // Not enough cached tokens; pull more from the lexer as needed.
            let remaining = n - self.lookahead.len() + 1;
            self.lookahead.reserve(remaining);
            for _ in 0..remaining {
                let tok = self.next_valid_token();
                self.lookahead.push_back(tok);
            }
        }
        self.lookahead[n]
    }

    pub(crate) fn nth_non_space(&mut self, n: usize) -> Token {
        // Check if there's enough cached non-whitespace tokens to answer the
        // query.
        let mut processed = 0;
        for tok in self
            .lookahead
            .iter()
            .filter(|tok| tok.kind != TokenKind::Whitespace)
        {
            if processed == n {
                return *tok;
            }
            processed += 1;
        }

        // Not enough cached tokens; pull more from the lexer as needed.
        let mut remaining = n - processed;
        self.lookahead.reserve(remaining);
        loop {
            let tok = self.next_valid_token();
            self.lookahead.push_back(tok);
            if tok.kind != TokenKind::Whitespace {
                if remaining == 0 {
                    return tok;
                }
                remaining -= 1;
            }
        }
    }

    pub(crate) fn bump(&mut self) {
        self.lookahead.pop_front();
    }

    pub(crate) fn bump_skip_spaces(&mut self) {
        while let Some(tok) = self.lookahead.pop_front() {
            if tok.kind == TokenKind::Whitespace {
                break;
            }
        }
    }

    fn next_valid_token(&mut self) -> Token {
        loop {
            let tok = self.lexer.next_token();
            if tok.kind != TokenKind::Invalid {
                return tok;
            }
        }
    }
}

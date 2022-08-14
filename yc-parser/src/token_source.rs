use std::collections::VecDeque;

use yc_ast::token::{Token, TokenKind};

use crate::lex::Lexer;

pub(crate) struct TokenSource<'a> {
    lexer: Lexer<'a>,
    lookahead: VecDeque<Token>,
}

impl<'a> TokenSource<'a> {
    pub(crate) fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            lookahead: VecDeque::new(),
        }
    }

    pub(crate) fn next(&mut self) -> Token {
        self.lookahead
            .pop_front()
            .unwrap_or_else(|| self.lexer.next_token())
    }

    pub(crate) fn next_non_space(&mut self) -> Token {
        while let Some(token) = self.lookahead.pop_front() {
            if !matches!(token.kind, TokenKind::Whitespace(_)) {
                return token;
            }
        }

        self.lexer
            .tokens()
            .find(|token| !matches!(token.kind, TokenKind::Whitespace(_)))
            .unwrap()
    }

    pub(crate) fn peek(&mut self) -> &Token {
        self.peek_nth(0)
    }

    pub(crate) fn peek_nth(&mut self, n: usize) -> &Token {
        if self.lookahead.len() <= n {
            self.lookahead
                .extend(self.lexer.tokens().take(n - self.lookahead.len() + 1));
        };
        &self.lookahead[n - 1]
    }

    pub(crate) fn source(&self) -> &'a str {
        self.lexer.source()
    }
}

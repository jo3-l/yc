use std::collections::VecDeque;

use yc_ast::token::{Token, TokenKind};

use crate::lex::Lexer;

/// A source of tokens used by the parser that supports infinite lookahead.
pub(crate) struct TokenSource<'src> {
    lexer: Lexer<'src>,
    lookahead: VecDeque<Token>,
}

impl<'src> TokenSource<'src> {
    pub(crate) fn new(lexer: Lexer<'src>) -> Self {
        Self {
            lexer,
            lookahead: VecDeque::new(),
        }
    }

    /// Returns the next valid non-whitespace token.
    pub(crate) fn next(&mut self) -> Token {
        while let Some(token) = self.lookahead.pop_front() {
            if !matches!(token.kind, TokenKind::Whitespace(_)) {
                return token;
            }
        }

        self.lexer
            .tokens()
            .find(|token| !matches!(token.kind, TokenKind::Whitespace(_)))
            // Should never panic because tokens() returns TokenKind::Eof
            // endlessly when the end of text is reached, so there will always
            // be a non-whitespace token.
            .unwrap()
    }

    /// Retrieves but does not consume the next valid non-whitespace token.
    pub(crate) fn peek(&mut self) -> &Token {
        self.peek_nth(0)
    }

    /// Retrieves but does not consume the `n`th valid non-whitespace token
    /// starting from the current token, where `n` starts at `0`.
    pub(crate) fn peek_nth(&mut self, n: usize) -> &Token {
        if self.lookahead.len() <= n {
            self.lookahead.extend(
                self.lexer
                    .tokens()
                    .filter(|token| {
                        !matches!(token.kind, TokenKind::Invalid | TokenKind::Whitespace(_))
                    })
                    .take(n - self.lookahead.len() + 1),
            );
        };

        // At this point, the lookahead buffer should always contain at least n
        // + 1 elements due to the branch above.
        &self.lookahead[n]
    }

    pub(crate) fn source(&self) -> &'src str {
        self.lexer.source()
    }
}

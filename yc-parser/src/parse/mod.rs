mod action;
mod expr;
mod lit;
mod state;
mod token_cursor;

use yc_ast::ast;
use yc_ast::node_id::NodeId;
use yc_ast::token::{Token, TokenKind};
use yc_diagnostics::{Diagnostic, FileId};

use crate::lex::Lexer;
use crate::parse::token_cursor::TokenCursor;

pub struct Parser<'src> {
    file_id: FileId,
    cursor: TokenCursor<'src>,
    diagnostics: Vec<Diagnostic>,
    node_id: NodeId,
}

impl<'src> Parser<'src> {
    pub fn new(file_id: FileId, lexer: Lexer<'src>) -> Self {
        Self {
            file_id,
            cursor: TokenCursor::new(lexer),
            diagnostics: vec![],
            node_id: NodeId::from_usize(0),
        }
    }

    pub fn parse_root(mut self) -> (ast::Root, Vec<Diagnostic>) {
        todo!()
    }

    pub fn source(&self) -> &'src str {
        self.cursor.source()
    }

    pub(crate) fn error_unexpected(
        &self,
        tok: Token,
        expected: Option<impl Into<String>>,
    ) -> Diagnostic {
        let msg = match expected {
            Some(expected) => format!(
                "expected {} but instead found {}",
                expected.into(),
                tok.kind.describe()
            ),
            None => format!("unexpected token {}", tok.kind.describe()),
        };
        let mut err = Diagnostic::error(self.file_id, msg).with_primary_span(tok.span);
        if tok.kind == TokenKind::Comment {
            err = err.with_footer_note("note: comments cannot be inserted inside other actions");
        }
        err
    }

    pub(crate) fn at(&mut self, pat: impl TokenPattern) -> bool {
        pat.matches(self.cursor.first())
    }

    pub(crate) fn at_ignore_spaces(&mut self, pat: impl TokenPattern) -> bool {
        pat.matches(self.cursor.first_non_space())
    }

    pub(crate) fn nth_at(&mut self, n: usize, pat: impl TokenPattern) -> bool {
        pat.matches(self.cursor.nth(n))
    }

    pub(crate) fn nth_at_ignore_spaces(&mut self, n: usize, pat: impl TokenPattern) -> bool {
        pat.matches(self.cursor.nth_non_space(n))
    }

    pub(crate) fn eat(&mut self, pat: impl TokenPattern) -> bool {
        if pat.matches(self.cursor.first()) {
            self.cursor.bump();
            true
        } else {
            false
        }
    }

    pub(crate) fn must_eat(&mut self, pat: impl TokenPattern) -> Token {
        let tok = self.cursor.first();
        assert!(self.eat(pat));
        tok
    }

    pub(crate) fn eat_skip_spaces(&mut self, pat: impl TokenPattern) -> bool {
        if pat.matches(self.cursor.first_non_space()) {
            self.cursor.bump();
            true
        } else {
            false
        }
    }

    pub(crate) fn must_eat_skip_spaces(&mut self, pat: impl TokenPattern) -> Token {
        let tok = self.cursor.first_non_space();
        assert!(self.eat_skip_spaces(pat));
        tok
    }

    pub(crate) fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub(crate) fn next_node_id(&mut self) -> NodeId {
        self.node_id += 1;
        self.node_id
    }
}

pub(crate) trait TokenPattern {
    fn matches(self, token: Token) -> bool;
}

impl TokenPattern for TokenKind {
    fn matches(self, token: Token) -> bool {
        token.kind == self
    }
}

impl<'a, const N: usize> TokenPattern for &'a [TokenKind; N] {
    fn matches(self, token: Token) -> bool {
        self.contains(&token.kind)
    }
}

mod action;
mod expr;
mod lit;
mod token_cursor;

use yc_ast::ast;
use yc_ast::node_id::NodeId;
use yc_ast::token::{Token, TokenKind};
use yc_diagnostics::Diagnostic;

use crate::lex::Lexer;
use crate::parse::token_cursor::TokenCursor;

pub struct Parser<'src> {
    cursor: TokenCursor<'src>,
    diagnostics: Vec<Diagnostic>,
    node_id: NodeId,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        Self {
            cursor: TokenCursor::new(lexer),
            diagnostics: vec![],
            node_id: NodeId::from_usize(0),
        }
    }

    pub fn parse_root(mut self) -> (ast::Root, Vec<Diagnostic>) {
        todo!()
    }

    fn at(&mut self, pat: impl TokenPattern) -> bool {
        pat.matches(self.cursor.first())
    }

    fn at_ignore_spaces(&mut self, pat: impl TokenPattern) -> bool {
        pat.matches(self.cursor.first_non_space())
    }

    fn nth_at(&mut self, n: usize, pat: impl TokenPattern) -> bool {
        pat.matches(self.cursor.nth(n))
    }

    fn nth_at_ignore_spaces(&mut self, n: usize, pat: impl TokenPattern) -> bool {
        pat.matches(self.cursor.nth_non_space(n))
    }

    fn eat(&mut self, pat: impl TokenPattern) -> bool {
        if pat.matches(self.cursor.first()) {
            self.cursor.bump();
            true
        } else {
            false
        }
    }

    fn eat_ignore_spaces(&mut self, pat: impl TokenPattern) -> bool {
        if pat.matches(self.cursor.first_non_space()) {
            self.cursor.bump();
            true
        } else {
            false
        }
    }

    fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn next_node_id(&mut self) -> NodeId {
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

#[derive(Clone, Debug, Default)]
struct ParseState {
    defined_vars: Vec<String>,
    loop_depth: usize,
}

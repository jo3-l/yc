mod token_cursor;

use yc_ast::ast;
use yc_ast::location::BytePos;
use yc_ast::node_id::NodeId;
use yc_diagnostics::Diagnostic;

use crate::lex::Lexer;
use crate::parse::token_cursor::TokenCursor;

pub struct Parser<'src> {
    tokens: TokenCursor<'src>,
    diagnostics: Vec<Diagnostic>,
    node_id: NodeId,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        Self {
            tokens: TokenCursor::new(lexer),
            diagnostics: vec![],
            node_id: NodeId::from_usize(0),
        }
    }

    pub fn parse_root(mut self) -> (ast::Root, Vec<Diagnostic>) {
        todo!()
    }

    fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn next_node_id(&mut self) -> NodeId {
        self.node_id += 1;
        self.node_id
    }
}

#[derive(Clone, Debug, Default)]
struct ParseState {
    defined_vars: Vec<String>,
    loop_depth: usize,
}

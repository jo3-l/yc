use yc_ast::ast;
use yc_ast::location::BytePos;
use yc_ast::node_id::NodeId;
use yc_diagnostics::Diagnostic;

use crate::lex::Lexer;
use crate::token_source::TokenSource;

pub struct Parser<'src> {
    tokens: TokenSource<'src>,
    diagnostics: Vec<Diagnostic>,
    node_id: NodeId,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Self {
        Self {
            tokens: TokenSource::new(lexer),
            diagnostics: vec![],
            node_id: NodeId::from_usize(0),
        }
    }

    pub fn parse_root(mut self) -> (ast::Root, Vec<Diagnostic>) {
        todo!()
    }

    fn parse_actions(&mut self, ctx: &mut ParseContext) -> Vec<ast::Action> {
        todo!()
    }

    fn parse_block_action(
        &mut self,
        start_pos: BytePos,
        trim_markers: ast::TrimMarkers,
        ctx: &mut ParseContext,
    ) -> ast::BlockAction {
        todo!()
    }

    fn parse_define_action(
        &mut self,
        start_pos: BytePos,
        trim_markers: ast::TrimMarkers,
        ctx: &mut ParseContext,
    ) -> ast::DefineAction {
        todo!()
    }

    fn parse_conditional_branch_action(
        &mut self,
        kind: ast::BranchKind,
        start_pos: BytePos,
        trim_markers: ast::TrimMarkers,
        ctx: &mut ParseContext,
    ) -> ast::ConditionalBranchAction {
        todo!()
    }

    fn parse_range_action(
        &mut self,
        start_pos: BytePos,
        trim_markers: ast::TrimMarkers,
        ctx: &mut ParseContext,
    ) -> ast::RangeAction {
        todo!()
    }

    fn parse_while_action(
        &mut self,
        start_pos: BytePos,
        trim_markers: ast::TrimMarkers,
        ctx: &mut ParseContext,
    ) -> ast::WhileAction {
        todo!()
    }

    fn parse_loop_control_action(
        &mut self,
        kind: ast::LoopControlKind,
        start_pos: BytePos,
        trim_markers: ast::TrimMarkers,
        ctx: &mut ParseContext,
    ) -> ast::LoopControlAction {
        todo!()
    }

    fn parse_try_action(
        &mut self,
        start_pos: BytePos,
        trim_markers: ast::TrimMarkers,
        ctx: &mut ParseContext,
    ) -> ast::TryAction {
        todo!()
    }

    fn parse_return_action(
        &mut self,
        start_pos: BytePos,
        trim_markers: ast::TrimMarkers,
        ctx: &mut ParseContext,
    ) -> ast::ReturnAction {
        todo!()
    }

    fn parse_expr(&mut self, ctx: &mut ParseContext) -> ast::Expr {
        todo!()
    }

    fn parse_fn_call_expr(
        &mut self,
        start_pos: BytePos,
        name: &str,
        ctx: &mut ParseContext,
    ) -> ast::FnCallExpr {
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
struct ParseContext {
    defined_vars: Vec<String>,
    loop_depth: usize,
}

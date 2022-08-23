use yc_ast::ast::{self, Expr, Ident, SelectorTarget};
use yc_ast::location::{BytePos, Span};
use yc_ast::parsed_fragment::ParsedFragment;
use yc_ast::token::{Token, TokenKind};
use yc_diagnostics::Diagnostic;

use crate::parse::state::ParseState;
use crate::parse::Parser;

impl Parser<'_> {
    pub(crate) fn parse_expr(&mut self, state: &mut ParseState) -> ParsedFragment<Expr> {
        let start_pos = self.cursor.pos_ignore_spaces();
        let init_expr = self.parse_non_pipeline_expr(state, ExprParseMode::Greedy);

        let mut calls = vec![];
        while self.eat_skip_spaces(TokenKind::Pipe) {
            let call_start_pos = self.cursor.pos_ignore_spaces();
            let expr = self.parse_non_pipeline_expr(state, ExprParseMode::Greedy);
            if let ParsedFragment::Present(Expr::FnCall(call)) = expr {
                calls.push(call);
            } else {
                let call_end_pos = self.cursor.pos();
                self.add_diagnostic(
                    Diagnostic::error(self.file_id, "expected function call after `|`")
                        .with_primary_span(Span::new(call_start_pos, call_end_pos)),
                );
            }
        }

        match calls.last() {
            Some(last_call) => ParsedFragment::Present(Expr::Pipeline(ast::ExprPipeline {
                id: self.next_node_id(),
                span: last_call.span.with_start(start_pos),
                init: init_expr.map(Box::new),
                calls,
            })),
            None => init_expr,
        }
    }

    fn parse_non_pipeline_expr(
        &mut self,
        state: &mut ParseState,
        mode: ExprParseMode,
    ) -> ParsedFragment<Expr> {
        let tok = self.cursor.first_non_space();
        let expr = match tok.kind {
            TokenKind::BoolLit => ParsedFragment::Present(Expr::BoolLit(self.parse_bool_lit())),
            TokenKind::CharLit => self.parse_char_lit().map(Expr::CharLit),
            TokenKind::FloatLit => self.parse_float_lit().map(Expr::FloatLit),
            TokenKind::IntLit => self.parse_int_lit().map(Expr::IntLit),
            TokenKind::Nil => ParsedFragment::Present(Expr::NilLit(self.parse_nil_lit())),
            TokenKind::QuotedStringLit => {
                ParsedFragment::Present(Expr::StringLit(self.parse_quoted_string_lit()))
            }
            TokenKind::RawStringLit => {
                ParsedFragment::Present(Expr::StringLit(self.parse_raw_string_lit()))
            }

            TokenKind::Variable => ParsedFragment::Present(self.parse_var_expr(state, mode)),
            TokenKind::Dot => {
                let dot = self.must_eat_skip_spaces(TokenKind::Dot);
                if self.at(TokenKind::Ident) {
                    ParsedFragment::Present(Expr::FieldAccessOrMethodCall(
                        self.parse_trailing_field_access_or_method_call(
                            state,
                            mode,
                            dot.span.start,
                            SelectorTarget::Context,
                        ),
                    ))
                } else {
                    ParsedFragment::Present(Expr::ContextAccess(ast::ContextAccessExpr {
                        id: self.next_node_id(),
                        span: dot.span,
                    }))
                }
            }
            TokenKind::Ident => {
                ParsedFragment::Present(Expr::FnCall(self.parse_fn_call(state, mode)))
            }
            TokenKind::LeftParen => {
                let left_paren = self.must_eat_skip_spaces(TokenKind::LeftParen);
                let expr = self.parse_expr(state);
                if !self.eat_skip_spaces(TokenKind::RightParen) {
                    self.add_diagnostic(
                        self.error_unexpected(tok, Some("`)`"))
                            .with_secondary_label(
                                left_paren.span,
                                "this left paren is never closed",
                            ),
                    );
                }
                expr
            }
            _ => {
                if !self.at_expr_terminator() {
                    // Make sure we don't get stuck at this position.
                    self.cursor.bump_skip_spaces();
                }
                self.add_diagnostic(self.error_unexpected(tok, Some("an expression")));
                ParsedFragment::Absent
            }
        };

        if self.eat(TokenKind::Dot) {
            ParsedFragment::Present(Expr::FieldAccessOrMethodCall(
                self.parse_trailing_field_access_or_method_call(
                    state,
                    mode,
                    tok.span.start,
                    SelectorTarget::Expr(expr.map(Box::new)),
                ),
            ))
        } else {
            expr
        }
    }

    fn parse_trailing_field_access_or_method_call(
        &mut self,
        state: &mut ParseState,
        mode: ExprParseMode,
        start_pos: BytePos,
        target: SelectorTarget,
    ) -> ast::FieldAccessOrMethodCallExpr {
        let selector = self.parse_selector();
        let mut field_or_method = ast::FieldAccessOrMethodCallExpr {
            id: self.next_node_id(),
            span: Span::new(start_pos, self.cursor.pos()),
            obj: target,
            selector,
            call_args: vec![],
        };

        while self.eat(TokenKind::Dot) {
            let selector = self.parse_selector();
            field_or_method = ast::FieldAccessOrMethodCallExpr {
                id: self.next_node_id(),
                span: field_or_method.span.with_end(self.cursor.pos()),
                obj: SelectorTarget::Expr(ParsedFragment::Present(Box::new(
                    Expr::FieldAccessOrMethodCall(field_or_method),
                ))),
                selector,
                call_args: vec![],
            }
        }

        if mode == ExprParseMode::Greedy {
            field_or_method.call_args = self.parse_call_args(state);
        }
        field_or_method
    }

    fn parse_selector(&mut self) -> ParsedFragment<Ident> {
        if self.at(TokenKind::Ident) {
            let ident = self.must_eat(TokenKind::Ident);
            let ident = self.tok_to_ident(ident);
            ParsedFragment::Present(ident)
        } else {
            ParsedFragment::Absent
        }
    }

    fn parse_var_expr(&mut self, state: &mut ParseState, mode: ExprParseMode) -> Expr {
        let var_name = self.must_eat_skip_spaces(TokenKind::Variable);
        let var_name = self.tok_to_var_name(var_name);
        if mode == ExprParseMode::Greedy {
            if self.eat_skip_spaces(TokenKind::Assign) {
                let expr = self.parse_expr(state);
                let span = var_name.span.with_end(self.cursor.pos());
                if !state.has_var(&var_name.val) {
                    self.add_diagnostic(
                        Diagnostic::error(
                            self.file_id,
                            format!("assignment to undefined variable ${}", var_name.val),
                        )
                        .with_primary_span(span),
                    );
                }

                return Expr::VarAssign(ast::VarAssignExpr {
                    id: self.next_node_id(),
                    span,
                    name: var_name,
                    expr: expr.map(Box::new),
                });
            } else if self.eat_skip_spaces(TokenKind::Declare) {
                let expr = self.parse_expr(state);
                let span = var_name.span.with_end(self.cursor.pos());
                state.push_var(&var_name.val); // TODO: Can we avoid the clone?

                return Expr::VarDecl(ast::VarDeclExpr {
                    id: self.next_node_id(),
                    span,
                    name: var_name,
                    expr: expr.map(Box::new),
                });
            }
        }

        if !state.has_var(&var_name.val) {
            self.add_diagnostic(
                Diagnostic::error(
                    self.file_id,
                    format!("undefined variable ${}", var_name.val),
                )
                .with_primary_span(var_name.span),
            );
        }
        Expr::VarRef(ast::VarRefExpr {
            id: self.next_node_id(),
            span: var_name.span,
            name: var_name,
        })
    }

    fn parse_fn_call(&mut self, state: &mut ParseState, mode: ExprParseMode) -> ast::FnCallExpr {
        let name = self.must_eat_skip_spaces(TokenKind::Ident);
        let name = self.tok_to_ident(name);
        let args = if mode == ExprParseMode::Greedy {
            self.parse_call_args(state)
        } else {
            vec![]
        };

        ast::FnCallExpr {
            id: self.next_node_id(),
            span: args
                .last()
                .map_or(name.span, |last_arg| name.span.to(last_arg.span())),
            name,
            args,
        }
    }

    fn parse_call_args(&mut self, state: &mut ParseState) -> Vec<Expr> {
        let mut args = vec![];
        while !self.at_expr_terminator() {
            if !self.eat(TokenKind::Whitespace) {
                let tok = self.cursor.first();
                self.error_unexpected(tok, Some("whitespace"));
            }

            if let ParsedFragment::Present(expr) =
                self.parse_non_pipeline_expr(state, ExprParseMode::Lazy)
            {
                args.push(expr);
            }
        }
        args
    }

    fn at_expr_terminator(&mut self) -> bool {
        self.at_ignore_spaces(&[
            TokenKind::RightTrimMarker,
            TokenKind::RightActionDelim,
            TokenKind::RightParen,
            TokenKind::Pipe,
        ])
    }

    fn tok_to_ident(&self, tok: Token) -> Ident {
        Ident {
            span: tok.span,
            val: self.source()[tok.span.as_range()].to_string(),
        }
    }

    fn tok_to_var_name(&self, tok: Token) -> ast::VarName {
        let val = &self.source()[tok.span.as_range()];
        ast::VarName {
            span: tok.span,
            val: val.strip_prefix('$').unwrap_or(val).to_string(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExprParseMode {
    /// Parse as many tokens as possible to form an expression, but stop at a
    /// pipe. For example, parsing `$x := 1 | fn $y` greedily would yield a
    /// declaration `$x := 1`.
    Greedy,
    /// Parse as few tokens as possible to form an expression. For example,
    /// parsing `fn1 1 2` would yield a function call `fn1` with no arguments.
    /// (The following numeric values `1` and `2` are assumed to be separate
    /// expressions.)
    Lazy,
}

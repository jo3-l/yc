// use crate::ast::lex::TokenKind;
// use crate::ast::token_source::TokenSource;
// use crate::ast::{self, Error, Span, Position, TrimMarkers};

// pub struct Parser<'a> {
//     tokens: TokenSource<'a>,
//     errors: Vec<Error>,
// }

// impl<'a> Parser<'a> {
//     pub(crate) fn new(tokens: TokenSource<'a>) -> Self {
//         Self {
//             tokens,
//             errors: vec![],
//         }
//     }

//     pub(crate) fn parse_root(mut self) -> ast::Root {
//         let mut ctx = ParseContext::default();
//     }

//     fn parse_stmts(&mut self, ctx: &mut ParseContext) -> Vec<ast::Stmt> {}

//     fn parse_block(&mut self) -> ast::BlockStmt {}

//     fn parse_define(&mut self) -> ast::DefineStmt {
//         let token = self.tokens.next_non_space();
//         let name = match self.parse_expr() {
//             ast::Expr::StringLit(ref lit) => lit,
//             expr => {
//                 self.error_at(expr.span(), "associated template name must be a string literal");
//             }
//         }
//     }

//     fn parse_conditional_branch(
//         &mut self,
//         kind: ast::ConditionalBranchStmt,
//     ) -> ast::ConditionalBranchStmt {
//     }

//     fn parse_range(&mut self) -> ast::RangeStmt {}

//     fn parse_while(&mut self) -> ast::WhileStmt {}

//     fn parse_loop_control(&mut self) -> ast::LoopControlStmt {}

//     fn parse_try(&mut self) -> ast::TryStmt {}

//     fn parse_return(&mut self) -> ast::ReturnStmt {}

//     fn parse_expr(&mut self) -> ast::Expr {}

//     fn parse_fn_call(&mut self) -> ast::FnCallExpr {}

//     fn parse_var(&mut self) -> ast::Expr {}

//     fn error_at<S>(&mut self, span: Span, message: S)
//     where
//         S: Into<String>,
//     {
//         self.errors.push(Error {
//             message: message.into(),
//             source_code: self.tokens.source().to_string(), // TODO: avoid copy?
//             span,
//         });
//     }
// }

// #[derive(Default)]
// struct ParseContext {
//     defined_vars: Vec<String>,
//     loop_depth: usize,
// }

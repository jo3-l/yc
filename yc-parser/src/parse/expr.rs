use crate::parse::{ParseState, Parser};

impl Parser<'_> {
    pub(crate) fn parse_expr(&mut self, mut state: &ParseState, mode: ExprParseMode) {
        todo!()
    }
}

pub(crate) enum ExprParseMode {
    /// Parse the expression as a pipeline. For example, parsing `$x = 5 | fn1
    /// $y (fn2 $z 5)` would yield a pipeline with the assignment `$x = 5` as
    /// the initial expression and the function call `fn1 $y (fn2 $z 5)` as
    /// the second stage.
    Pipeline,
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

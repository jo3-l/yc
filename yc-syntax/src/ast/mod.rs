use core::fmt;
use std::{cmp::Ordering, error, fmt::Display, ops::Range};

mod char_stream;
mod lex;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Error {
    pub message: String,
    pub source_code: String,
    pub span: Span,
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{}: {}",
            self.span.start.line, self.span.start.col, self.message
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn range(&self) -> Range<usize> {
        self.start.offset..self.end.offset
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Span) -> Ordering {
        (&self.start, &self.end).cmp(&(&other.start, &other.end))
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Range<usize> {
        span.range()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
}

impl Position {
    pub fn new(offset: usize, line: usize, col: usize) -> Self {
        Self { offset, line, col }
    }

    pub fn advance_by(&self, c: char) -> Position {
        let mut advanced = *self;
        advanced.offset += c.len_utf8();
        if c == '\n' {
            advanced.line += 1;
            advanced.col = 1;
        } else {
            advanced.col += 1;
        }
        advanced
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            offset: 0,
            line: 1,
            col: 1,
        }
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Position) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Position) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub stmts: Vec<Stmt>,
    pub comments: Vec<Comment>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Text(Text),

    Block(BlockStmt),
    Define(DefineStmt),
    ConditionalBranch(ConditionalBranchStmt),
    Range(RangeStmt),
    While(WhileStmt),
    LoopControl(LoopControlStmt),
    Try(TryStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Text {
    pub span: Span,
    pub content: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStmt {
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub context: Option<Expr>,
    pub body: Vec<Stmt>,
    pub end: EndStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DefineStmt {
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub name: StringLit,
    pub body: Vec<Stmt>,
    pub end: EndStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConditionalBranchStmt {
    pub kind: BranchKind,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub cond: Expr,
    pub body: Vec<Stmt>,
    pub else_branches: Vec<ElseBranch>,
    pub end: EndStmt,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BranchKind {
    If,
    With,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseBranch {
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub cond: Option<Expr>,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RangeStmt {
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub element_binding: Option<VarName>,
    pub index_binding: Option<VarName>,
    pub expr: Expr,
    pub body: Vec<Stmt>,
    pub end: EndStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStmt {
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub cond: Expr,
    pub body: Vec<Stmt>,
    pub end: EndStmt,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LoopControlStmt {
    pub span: Span,
    pub kind: LoopControlKind,
    pub trim_markers: TrimMarkers,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LoopControlKind {
    Break,
    Continue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TryStmt {
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub try_body: Vec<Stmt>,
    pub catch: CatchStmt,
    pub catch_body: Vec<Stmt>,
    pub end: EndStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CatchStmt {
    pub span: Span,
    pub trim_markers: TrimMarkers,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EndStmt {
    pub span: Span,
    pub trim_markers: TrimMarkers,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStmt {
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprStmt {
    pub span: Span,
    pub expr: Expr,
    pub trim_markers: TrimMarkers,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TrimMarkers {
    pub left: bool,
    pub right: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comment {
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub content: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    BoolLit(BoolLit),
    CharLit(CharLit),
    FloatLit(FloatLit),
    IntLit(IntLit),
    NilLit(NilLit),
    StringLit(StringLit),

    FnCall(FnCallExpr),
    Pipeline(ExprPipeline),
    VarAssign(VarAssignExpr),
    VarDef(VarDefExpr),
    VarRef(VarRefExpr),
}

macro_rules! define_lit {
    ($t:ty, $name:ident) => {
        #[derive(Clone, Debug, PartialEq)]
        pub struct $name {
            pub span: Span,
            pub val: $t,
        }
    };
}

define_lit!(bool, BoolLit);
define_lit!(char, CharLit);
define_lit!(f64, FloatLit);
define_lit!(i64, IntLit);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringLit {
    pub span: Span,
    pub kind: StringLitKind,
    pub val: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum StringLitKind {
    Raw,
    Interpreted,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NilLit {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarAssignExpr {
    pub span: Span,
    pub name: VarName,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDefExpr {
    pub span: Span,
    pub name: VarName,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarRefExpr {
    pub span: Span,
    pub name: VarName,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarName {
    pub span: Span,
    pub val: String,
}

#[derive(Clone, Debug, PartialEq)]

pub struct FnCallExpr {
    pub span: Span,
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprPipeline {
    pub init: Box<Expr>,
    pub fn_calls: Vec<FnCallExpr>,
}

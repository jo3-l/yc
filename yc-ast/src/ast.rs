use bitflags::bitflags;

use crate::location::Span;
use crate::node_id::NodeId;
use crate::parsed_fragment::ParsedFragment;

#[derive(Clone, Debug)]
pub struct Root {
    pub stmts: Vec<Stmt>,
    pub comments: Vec<Comment>,
}

#[derive(Clone, Debug)]
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

impl Stmt {
    pub fn span(&self) -> Span {
        use Stmt::*;
        match self {
            Text(ref text) => text.span,
            Block(ref block) => block.span,
            Define(ref definition) => definition.span,
            ConditionalBranch(ref branch) => branch.span,
            Range(ref range) => range.span,
            While(ref r#while) => r#while.span,
            LoopControl(ref r#loop) => r#loop.span,
            Try(ref r#try) => r#try.span,
            Return(ref r#return) => r#return.span,
            Expr(ref expr) => expr.span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Text {
    pub node_id: NodeId,
    pub span: Span,
    pub content: String,
}

#[derive(Clone, Debug)]
pub struct BlockStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub context: Option<Expr>,
    pub body: Vec<Stmt>,
    pub end: ParsedFragment<EndStmt>,
}

#[derive(Clone, Debug)]
pub struct DefineStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub name: StringLit,
    pub body: Vec<Stmt>,
    pub end: ParsedFragment<EndStmt>,
}

#[derive(Clone, Debug)]
pub struct ConditionalBranchStmt {
    pub node_id: NodeId,
    pub kind: BranchKind,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub cond: Expr,
    pub body: Vec<Stmt>,
    pub else_branches: Vec<ElseBranch>,
    pub end: ParsedFragment<EndStmt>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BranchKind {
    If,
    With,
}

#[derive(Clone, Debug)]
pub struct ElseBranch {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub cond: Option<Expr>,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct RangeStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub element_binding: Option<VarName>,
    pub index_binding: Option<VarName>,
    pub expr: Expr,
    pub body: Vec<Stmt>,
    pub end: ParsedFragment<EndStmt>,
}

#[derive(Clone, Debug)]
pub struct WhileStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub cond: Expr,
    pub body: Vec<Stmt>,
    pub end: ParsedFragment<EndStmt>,
}

#[derive(Clone, Debug)]
pub struct LoopControlStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub kind: LoopControlKind,
    pub trim_markers: TrimMarkers,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LoopControlKind {
    Break,
    Continue,
}

#[derive(Clone, Debug)]
pub struct TryStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub try_body: Vec<Stmt>,
    pub catch: ParsedFragment<CatchStmt>,
    pub catch_body: Vec<Stmt>,
    pub end: ParsedFragment<EndStmt>,
}

#[derive(Clone, Debug)]
pub struct CatchStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
}

#[derive(Clone, Debug)]
pub struct EndStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprStmt {
    pub node_id: NodeId,
    pub span: Span,
    pub expr: Expr,
    pub trim_markers: TrimMarkers,
}

bitflags! {
    pub struct TrimMarkers: u8 {
        const LEFT = 1 << 0;
        const RIGHT = 1 << 1;
    }
}

#[derive(Clone, Debug)]
pub struct Comment {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub content: String,
}

#[derive(Clone, Debug)]
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

    Invalid(InvalidExpr),
}

impl Expr {
    pub fn span(&self) -> Span {
        use Expr::*;
        match self {
            BoolLit(ref lit) => lit.span,
            CharLit(ref lit) => lit.span,
            FloatLit(ref lit) => lit.span,
            IntLit(ref lit) => lit.span,
            NilLit(ref lit) => lit.span,
            StringLit(ref lit) => lit.span,
            FnCall(ref call) => call.span,
            Pipeline(ref pipeline) => pipeline.span,
            VarAssign(ref assign) => assign.span,
            VarDef(ref def) => def.span,
            VarRef(ref r#ref) => r#ref.span,
            Invalid(ref invalid) => invalid.span,
        }
    }
}

macro_rules! define_lit {
    ($t:ty, $name:ident) => {
        #[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct StringLit {
    pub node_id: NodeId,
    pub span: Span,
    pub kind: StringLitKind,
    pub val: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum StringLitKind {
    Raw,
    Interpreted,
}

#[derive(Clone, Debug)]
pub struct NilLit {
    pub node_id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct VarAssignExpr {
    pub node_id: NodeId,
    pub span: Span,
    pub name: VarName,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct VarDefExpr {
    pub node_id: NodeId,
    pub span: Span,
    pub name: VarName,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct VarRefExpr {
    pub node_id: NodeId,
    pub span: Span,
    pub name: VarName,
}

#[derive(Clone, Debug)]
pub struct VarName {
    pub span: Span,
    pub val: String,
}

#[derive(Clone, Debug)]
pub struct FnCallExpr {
    pub node_id: NodeId,
    pub span: Span,
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprPipeline {
    pub node_id: NodeId,
    pub span: Span,
    pub init: Box<Expr>,
    pub fn_calls: Vec<FnCallExpr>,
}

#[derive(Clone, Debug)]
pub struct InvalidExpr {
    pub node_id: NodeId,
    pub span: Span,
}

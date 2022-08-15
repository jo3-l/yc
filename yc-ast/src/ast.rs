use bitflags::bitflags;

use crate::location::Span;
use crate::node_id::NodeId;
use crate::parsed_fragment::ParsedFragment;

#[derive(Clone, Debug)]
pub struct Root {
    pub stmts: Vec<Action>,
    pub comments: Vec<Comment>,
}

#[derive(Clone, Debug)]
pub enum Action {
    Text(Text),

    Block(BlockAction),
    Define(DefineAction),
    ConditionalBranch(ConditionalBranchAction),
    Range(RangeAction),
    While(WhileAction),
    LoopControl(LoopControlAction),
    Try(TryAction),
    Return(ReturnAction),
    Expr(ExprAction),
}

impl Action {
    pub fn span(&self) -> Span {
        use Action::*;
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

/// A piece of literal text.
#[derive(Clone, Debug)]
pub struct Text {
    pub node_id: NodeId,
    pub span: Span,
    pub content: String,
}

/// A `block` action.
///
/// ```text
/// {{block "template-name" expr}}
///     ...
/// {{end}}
/// ```
#[derive(Clone, Debug)]
pub struct BlockAction {
    pub node_id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{block}}` action.
    pub trim_markers: TrimMarkers,
    pub context: Option<Expr>,
    pub body: Vec<Action>,
    pub end: ParsedFragment<EndAction>,
}

/// A `define` action.
///
/// ```text
/// {{define "template-name"}}
///     ...
/// {{end}}
/// ```
#[derive(Clone, Debug)]
pub struct DefineAction {
    pub node_id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{define}}` action.
    pub trim_markers: TrimMarkers,
    pub name: StringLit,
    pub body: Vec<Action>,
    pub end: ParsedFragment<EndAction>,
}

/// A conditional branch, which is either an `if` action or a `with` action.
///
/// ```text
/// {{if expr}}
///     ..stmts
/// {{else if expr}}
///     ...stmts
/// {{end}}
/// ```
#[derive(Clone, Debug)]
pub struct ConditionalBranchAction {
    pub node_id: NodeId,
    pub kind: BranchKind,
    pub span: Span,
    /// The trim markers associated with the `{{if}}` action.
    pub trim_markers: TrimMarkers,
    pub cond: Expr,
    pub body: Vec<Action>,
    pub else_branches: Vec<ElseBranch>,
    pub end: ParsedFragment<EndAction>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BranchKind {
    If,
    With,
}

/// An `else` or `else if` branch of a conditional branch.
#[derive(Clone, Debug)]
pub struct ElseBranch {
    pub node_id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{else}}` action.
    pub trim_markers: TrimMarkers,
    pub cond: Option<Expr>,
    pub body: Vec<Action>,
}

/// A `range` action.
///
/// ```text
/// {{range expression}}
///    ...statements
/// {{else}}
///     ...statements
/// {{end}}
/// ```
#[derive(Clone, Debug)]
pub struct RangeAction {
    pub node_id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{range}}` action.
    pub trim_markers: TrimMarkers,
    pub element_binding: Option<VarName>,
    pub index_binding: Option<VarName>,
    pub expr: Expr,
    pub body: Vec<Action>,
    pub else_branch: Option<LoopElseBranch>,
    pub end: ParsedFragment<EndAction>,
}

/// A `while` action.
///
/// ```text
/// {{while expression}}
///     ...stmts
/// {{else}}
///     ...stmts
/// {{end}}
/// ```
#[derive(Clone, Debug)]
pub struct WhileAction {
    pub node_id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{while}}` action.
    pub trim_markers: TrimMarkers,
    pub cond: Expr,
    pub body: Vec<Action>,
    pub else_branch: Option<LoopElseBranch>,
    pub end: ParsedFragment<EndAction>,
}

/// The optional `else` branch of a `{{range}}` or `{{while}}` action.
#[derive(Clone, Debug)]
pub struct LoopElseBranch {
    pub node_id: NodeId,
    /// The trim markers associated with the `{{else}}` action.
    pub trim_markers: TrimMarkers,
    pub body: Vec<Action>,
}

/// A loop control action (either `{{break}}` or `{{continue}}`).
#[derive(Clone, Debug)]
pub struct LoopControlAction {
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

/// A `try` action.
///
/// ```text
/// {{try}}
///     ...stmts
/// {{catch}}
///     ...stmts
/// {{end}}
/// ```
#[derive(Clone, Debug)]
pub struct TryAction {
    pub node_id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{try}}` action.
    pub trim_markers: TrimMarkers,
    pub try_body: Vec<Action>,
    pub catch: ParsedFragment<CatchAction>,
    pub catch_body: Vec<Action>,
    pub end: ParsedFragment<EndAction>,
}

/// A `{{catch}}` action. This action always appears as part of a `{{try}}`
/// action in the AST, never alone.
#[derive(Clone, Debug)]
pub struct CatchAction {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
}

/// An `{{end}}` action. This action always appears as part of another actions
/// in the AST (e.g., an `{{if}}` action), never alone.
#[derive(Clone, Debug)]
pub struct EndAction {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
}

/// A `{{return}}` action.
///
/// ```text
/// {{return expr}}
/// ```
#[derive(Clone, Debug)]
pub struct ReturnAction {
    pub node_id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub expr: Expr,
}

/// An action containing an expression.
///
/// For example, `{{add 1 1}}` is an `ExprAction`, as `add 1 1` is a
/// `FnCallExpr`.
#[derive(Clone, Debug)]
pub struct ExprAction {
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
            BoolLit(ref bool_lit) => bool_lit.span,
            CharLit(ref char_lit) => char_lit.span,
            FloatLit(ref float_lit) => float_lit.span,
            IntLit(ref int_lit) => int_lit.span,
            NilLit(ref nil_lit) => nil_lit.span,
            StringLit(ref string_lit) => string_lit.span,
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

/// A variable assignment expression, evaluating to the new value of the
/// variable.
///
/// ```text
/// $var_name = expr
/// ```
#[derive(Clone, Debug)]
pub struct VarAssignExpr {
    pub node_id: NodeId,
    pub span: Span,
    pub name: VarName,
    pub expr: Box<Expr>,
}

/// A variable definition expression, evaluating to the initializer.
///
/// ```text
/// $var_name := expr
/// ```
#[derive(Clone, Debug)]
pub struct VarDefExpr {
    pub node_id: NodeId,
    pub span: Span,
    pub name: VarName,
    pub expr: Box<Expr>,
}

/// A reference to a variable.
///
/// ```text
/// $var_name
/// ```
#[derive(Clone, Debug)]
pub struct VarRefExpr {
    pub node_id: NodeId,
    pub span: Span,
    pub name: VarName,
}

/// A variable name. This action always appears as part of another actions in
/// the AST, never alone.
#[derive(Clone, Debug)]
pub struct VarName {
    pub span: Span,
    pub val: String,
}

/// A call expression, evaluating to the result of the function call.
///
/// ```text
/// fn_name arg0 arg1 ... arg_n
/// ```
#[derive(Clone, Debug)]
pub struct FnCallExpr {
    pub node_id: NodeId,
    pub span: Span,
    pub name: String,
    pub args: Vec<Expr>,
}

/// A pipeline of expressions.
///
/// ```text
/// init_expr | fn0 arg0 arg1 | fn1 arg0 arg1 | ... | fn_n
/// ```
#[derive(Clone, Debug)]
pub struct ExprPipeline {
    pub node_id: NodeId,
    pub span: Span,
    pub init: Box<Expr>,
    pub fn_calls: Vec<FnCallExpr>,
}

/// An erroneous expression.
#[derive(Clone, Debug)]
pub struct InvalidExpr {
    pub node_id: NodeId,
    pub span: Span,
}

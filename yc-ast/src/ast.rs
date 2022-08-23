use bitflags::bitflags;

use crate::location::Span;
use crate::node_id::NodeId;
use crate::parsed_fragment::ParsedFragment;

/// The root of the AST.
///
/// ```text
/// Program = { Action | Comment } .
/// ```
#[derive(Clone, Debug)]
pub struct Root {
    pub stmts: Vec<Action>,
    pub comments: Vec<Comment>,
}

/// A top-level action.
///
/// ```text
/// Action = Text |
///     BlockAction |
///     ConditionalBranchAction |
///     RangeAction |
///     WhileAction |
///     LoopControlAction |
///     TryAction |
///     ReturnAction |
///     ExprAction .
/// ````
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

/// A piece of literal text to be outputted. All characters that are not part of an
/// action are considered to be text.
#[derive(Clone, Debug)]
pub struct Text {
    pub id: NodeId,
    pub span: Span,
    pub content: String,
}

/// A `block` action.
///
/// ```text
/// BlockAction = left_delim "block" StringLit [ Expr ] right_delim
///     { Action }
///     EndOfAction .
/// ```
#[derive(Clone, Debug)]
pub struct BlockAction {
    pub id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{block}}` action.
    pub trim_markers: TrimMarkers,
    pub context: ParsedFragment<Expr>,
    pub body: Vec<Action>,
    pub end: ParsedFragment<EndOfAction>,
}

/// A `define` action.
///
/// ```text
/// DefineAction = left_delim "define" StringLit right_delim
///     { Action }
///     EndOfAction .
/// ```
#[derive(Clone, Debug)]
pub struct DefineAction {
    pub id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{define}}` action.
    pub trim_markers: TrimMarkers,
    pub name: StringLit,
    pub body: Vec<Action>,
    pub end: ParsedFragment<EndOfAction>,
}

/// A conditional branch, which is either an `if` action or a `with` action.
///
/// ```text
/// ConditionalBranchAction = left_delim ( "if" | "with" ) Expr right_delim
///     { Action }
///     { ElseBranch }
///     EndOfAction .
/// ```
#[derive(Clone, Debug)]
pub struct ConditionalBranchAction {
    pub id: NodeId,
    pub kind: BranchKind,
    pub span: Span,
    /// The trim markers associated with the `{{if}}` action.
    pub trim_markers: TrimMarkers,
    pub cond: Expr,
    pub body: Vec<Action>,
    pub else_branches: Vec<ConditionalElseBranch>,
    pub end: ParsedFragment<EndOfAction>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BranchKind {
    If,
    With,
}

/// An `else` or `else if` branch of a conditional action.
///
/// ```text
/// ConditionalElseBranch = left_delim "else" [ "if" Expr ] right_delim
///     { Action } .
/// ```
#[derive(Clone, Debug)]
pub struct ConditionalElseBranch {
    pub id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{else}}` action.
    pub trim_markers: TrimMarkers,
    pub cond: ParsedFragment<Expr>,
    pub body: Vec<Action>,
}

/// A `range` action.
///
/// ```text
/// RangeAction = left_delim "range" RangeInitializer right_delim
///     { Action }
///     [ LoopElseBranch ]
///     EndOfAction .
/// RangeInitializer = VarName "," VarName ( ":=" | "=" ) Expr |
///     VarName ( ":=" | "=" ) Expr |
///     Expr .
/// ```
#[derive(Clone, Debug)]
pub struct RangeAction {
    pub id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{range}}` action.
    pub trim_markers: TrimMarkers,
    pub element_binding: Option<VarName>,
    pub index_binding: Option<VarName>,
    pub expr: ParsedFragment<Expr>,
    pub body: Vec<Action>,
    pub else_branch: Option<LoopElseBranch>,
    pub end: ParsedFragment<EndOfAction>,
}

/// A `while` action.
///
/// ```text
/// WhileAction = left_delim "while" Expr right_delim
///     { Action }
///     [ LoopElseBranch ]
///     EndOfAction .
/// ```
#[derive(Clone, Debug)]
pub struct WhileAction {
    pub id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{while}}` action.
    pub trim_markers: TrimMarkers,
    pub cond: Expr,
    pub body: Vec<Action>,
    pub else_branch: Option<LoopElseBranch>,
    pub end: ParsedFragment<EndOfAction>,
}

/// The optional `{{else}}` branch of a `{{range}}` or `{{while}}` action.
///
/// ```text
/// LoopElseBranch = left_delim "else" right_delim
///     { Action } .
/// ```
#[derive(Clone, Debug)]
pub struct LoopElseBranch {
    pub id: NodeId,
    /// The trim markers associated with the `{{else}}` action.
    pub trim_markers: TrimMarkers,
    pub body: Vec<Action>,
}

/// A loop control action (either `{{break}}` or `{{continue}}`.)
///
/// ```text
/// LoopControlAction = left_delim ( "break" | "continue" ) right_delim .
/// ```
#[derive(Clone, Debug)]
pub struct LoopControlAction {
    pub id: NodeId,
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
/// TryAction = left_delim "try" right_delim
///     { Action }
///     CatchAction
///     { Action }
///     EndOfAction .
/// ```
#[derive(Clone, Debug)]
pub struct TryAction {
    pub id: NodeId,
    pub span: Span,
    /// The trim markers associated with the `{{try}}` action.
    pub trim_markers: TrimMarkers,
    pub try_body: Vec<Action>,
    pub catch: ParsedFragment<CatchAction>,
    pub catch_body: Vec<Action>,
    pub end: ParsedFragment<EndOfAction>,
}

/// A `{{catch}}` action.
///
/// ```text
/// CatchAction = left_delim "catch" right_delim .
/// ```
#[derive(Clone, Debug)]
pub struct CatchAction {
    pub id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
}

/// An `{{end}}` action.
///
/// ```text
/// EndOfAction = left_delim "end" right_delim .
/// ```
#[derive(Clone, Debug)]
pub struct EndOfAction {
    pub id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
}

/// A `{{return}}` action. Return actions are permissible at the top level of a
/// program and result in execution being terminated at that point.
///
/// ```text
/// ReturnAction = left_delim "return" [ Expr ] right_delim .
/// ```
#[derive(Clone, Debug)]
pub struct ReturnAction {
    pub id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub expr: Option<Expr>,
}

/// An action containing a single expression. For example, `{{add 1 1}}` is
/// an `ExprAction`, as `add 1 1` is a `FnCallExpr`.
///
/// ```text
/// ExprAction = left_delim Expr right_delim .
/// ```
#[derive(Clone, Debug)]
pub struct ExprAction {
    pub id: NodeId,
    pub span: Span,
    pub expr: Expr,
    pub trim_markers: TrimMarkers,
}

bitflags! {
    /// Markers that configure trimming of output before and after an action.
    ///
    /// ```text
    /// left_delim        = "{{" [ left_trim_marker ] .
    /// right_delim       = "[ right_trim_marker ] "}}" .
    /// left_trim_marker  = "- " .
    /// right_trim_marker = " -" .
    /// ```
    pub struct TrimMarkers: u8 {
        const LEFT = 1 << 0;
        const RIGHT = 1 << 1;
    }
}

/// A comment. Comments start with a left action delimiter followed by a slash
/// and an asterisk and conclude with an asterisk and a slash followed by a
/// right action delimiter. It is not permissible for comments to nest, nor may
/// they appear as part of other actions.
#[derive(Clone, Debug)]
pub struct Comment {
    pub id: NodeId,
    pub span: Span,
    pub trim_markers: TrimMarkers,
    pub content: String,
}

/// An expression within an action.
///
/// ```text
/// Expr = BoolLit |
///     CharLit |
///     FloatLit |
///     IntLit |
///     NilLit |
///     StringLit |
///     ContextAccessExpr |
///     FieldAccessOrMethodCallExpr |
///     FnCallExpr |
///     ExprPipeline |
///     VarAssignExpr |
///     VarDeclExpr |
///     VarRefExpr .
/// ```
#[derive(Clone, Debug)]
pub enum Expr {
    BoolLit(BoolLit),
    CharLit(CharLit),
    FloatLit(FloatLit),
    IntLit(IntLit),
    NilLit(NilLit),
    StringLit(StringLit),

    ContextAccess(ContextAccessExpr),
    FieldAccessOrMethodCall(FieldAccessOrMethodCallExpr),
    FnCall(FnCallExpr),
    Pipeline(ExprPipeline),
    VarAssign(VarAssignExpr),
    VarDecl(VarDeclExpr),
    VarRef(VarRefExpr),
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

            ContextAccess(ref ctx) => ctx.span,
            FieldAccessOrMethodCall(ref field_or_method) => field_or_method.span,
            FnCall(ref call) => call.span,
            Pipeline(ref pipeline) => pipeline.span,
            VarAssign(ref assign) => assign.span,
            VarDecl(ref def) => def.span,
            VarRef(ref r#ref) => r#ref.span,
        }
    }
}

macro_rules! define_lit {
    ($(#[$attr:meta])* struct $name:ident : $t:ty;) => {
        $(#[$attr])*
        #[derive(Clone, Debug)]
        pub struct $name {
            pub id: NodeId,
            pub span: Span,
            pub val: $t,
        }
    };
}

define_lit! {
    /// A boolean literal.
    ///
    /// ```text
    /// BoolLit = "true" | "false" .
    /// ```
    struct BoolLit: bool;
}

define_lit! {
    /// A character literal, with the same syntax as [its Go
    /// equivalent](https://go.dev/ref/spec#Rune_literals).
    struct CharLit: char;
}

define_lit! {
    /// A floating-point literal, with the same syntax as [its Go
    /// equivalent](https://go.dev/ref/spec#Floating-point_literals).
    struct FloatLit: f64;
}

define_lit! {
    /// An integer literal, with the same syntax as [its Go
    /// equivalent](https://go.dev/ref/spec#Integer_literals).
    struct IntLit: i64;
}

/// A string literal, with the same syntax as [its Go
/// equivalent](https://go.dev/ref/spec#String_literals).
#[derive(Clone, Debug)]
pub struct StringLit {
    pub id: NodeId,
    pub span: Span,
    pub kind: StringLitKind,
    pub val: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum StringLitKind {
    Raw,
    Interpreted,
}

/// A nil literal.
///
/// ```text
/// NilLit = "nil" .
/// ```
#[derive(Clone, Debug)]
pub struct NilLit {
    pub id: NodeId,
    pub span: Span,
}

/// A context access expression.
///
/// ```text
/// ContextAccessExpr = "." .
/// ```
#[derive(Clone, Debug)]
pub struct ContextAccessExpr {
    pub id: NodeId,
    pub span: Span,
}

/// A field access or method call expression.
///
/// ```text
/// FieldAccessOrMethodCallExpr = [ Expr ] "." ident { Expr } .
/// ```
#[derive(Clone, Debug)]
pub struct FieldAccessOrMethodCallExpr {
    pub id: NodeId,
    pub span: Span,
    pub obj: SelectorTarget,
    pub selector: ParsedFragment<Ident>,
    pub call_args: Vec<Expr>,
}

/// The target of a selector expression. For example, the target of `(f arg0
/// arg1).Y` is `(f arg0 arg1)`, whereas the target of `.Y` is the context
/// value.
#[derive(Clone, Debug)]
pub enum SelectorTarget {
    Context,
    Expr(ParsedFragment<Box<Expr>>),
}

/// A variable assignment expression.
///
/// ```text
/// VarAssignExpr = VarName "=" Expr .
/// ```
#[derive(Clone, Debug)]
pub struct VarAssignExpr {
    pub id: NodeId,
    pub span: Span,
    pub name: VarName,
    pub expr: ParsedFragment<Box<Expr>>,
}

/// A variable declaration expression.
///
/// ```text
/// VarDeclExpr = VarName ":=" Expr .
/// ```
#[derive(Clone, Debug)]
pub struct VarDeclExpr {
    pub id: NodeId,
    pub span: Span,
    pub name: VarName,
    pub expr: ParsedFragment<Box<Expr>>,
}

/// A reference to a variable.
///
/// ```text
/// VarRefExpr = VarName .
/// ```
#[derive(Clone, Debug)]
pub struct VarRefExpr {
    pub id: NodeId,
    pub span: Span,
    pub name: VarName,
}

/// A variable name.
///
/// ```text
/// VarName = "$" ident .
/// ```
#[derive(Clone, Debug)]
pub struct VarName {
    pub span: Span,
    pub val: String,
}

/// A call expression.
///
/// ```text
/// FnCallExpr = ident { Expr } .
/// ```
#[derive(Clone, Debug)]
pub struct FnCallExpr {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub args: Vec<Expr>,
}

/// An alphanumeric identifier.
#[derive(Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub val: String,
}

/// A pipeline of expressions.
///
/// ```text
/// ExprPipeline  = Expr PipelineStage { PipelineStage } .
/// PipelineStage = "|" FnCallExpr .
/// ```
#[derive(Clone, Debug)]
pub struct ExprPipeline {
    pub id: NodeId,
    pub span: Span,
    pub init: ParsedFragment<Box<Expr>>,
    pub calls: Vec<FnCallExpr>,
}

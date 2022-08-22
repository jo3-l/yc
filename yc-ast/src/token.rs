use std::fmt::Debug;

use crate::location::Span;

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} @ {:?}", self.kind, self.span)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Text,

    BoolLit,
    CharLit,
    FloatLit,
    IntLit,
    QuotedStringLit,
    RawStringLit,

    Assign,
    Comma,
    Comment,
    Declare,
    Ident,
    LeftActionDelim,
    LeftTrimMarker,
    LeftParen,
    Pipe,
    RightActionDelim,
    RightTrimMarker,
    RightParen,
    Variable,
    Whitespace,

    Block,
    Break,
    Catch,
    Continue,
    Define,
    Dot,
    Else,
    End,
    If,
    Nil,
    Range,
    Return,
    Template,
    Try,
    While,
    With,

    Invalid,
    Eof,
}

impl TokenKind {
    /// Returns a string representation of this token kind suitable for usage in
    /// diagnostics.
    pub fn describe(&self) -> &str {
        use TokenKind::*;
        match self {
            Text => "text",

            BoolLit => "boolean literal",
            CharLit => "character literal",
            FloatLit => "floating-point literal",
            IntLit => "integer literal",
            QuotedStringLit => "quoted string literal",
            RawStringLit => "raw string literal",

            Assign => "`=`",
            Comma => "`,`",
            Comment => "comment",
            Declare => "`:=`",
            Ident => "identifier",
            LeftActionDelim => "`{{`",
            LeftTrimMarker => "`- `",
            LeftParen => "`(`",
            Pipe => "`|`",
            RightActionDelim => "`}}`",
            RightTrimMarker => "` -`",
            RightParen => "`)`",
            Variable => "variable",
            Whitespace => "whitespace",

            Block => "`block`",
            Break => "`break`",
            Catch => "`catch`",
            Continue => "`continue`",
            Define => "`define`",
            Dot => "`.`",
            Else => "`else`",
            End => "`end`",
            If => "`if`",
            Nil => "`nil`",
            Range => "`range`",
            Return => "`return`",
            Template => "`template`",
            Try => "`try`",
            While => "`while`",
            With => "`with`",

            Invalid => "<invalid>",
            Eof => "EOF",
        }
    }
}

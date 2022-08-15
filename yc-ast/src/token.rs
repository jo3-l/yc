use crate::location::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    BoolLiteral(bool),
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(i64),

    /// `=` introducing an assignment.
    Assign,
    Comma,
    /// A comment. The value is the body of the comment, starting just after the
    /// left delimiter and ending before the right delimiter.
    Comment(String),
    /// `:=` introducing a declaration.
    Declare,
    /// An alphanumeric identifier starting with a dot. The value does not
    /// include the dot.
    Field(String),
    /// An alphanumeric identifier not starting with a dot.
    Ident(String),
    LeftActionDelim(/* has_trim_marker */ bool),
    LeftParen,
    Pipe,
    /// A raw string literal. The value is the content of the string, not
    /// including the delimiters.
    RawString(String),
    RightActionDelim(/* has_trim_marker */ bool),
    RightParen,
    /// A quoted string literal. The value is the interpreted content of the
    /// string, not including the delimiters.
    String(String),
    Text(String),
    /// A variable. The value is the name of the variable, not including the '$'.
    Variable(String),
    /// Run of spaces separating expressions in a statement.
    Whitespace(String),

    Block,
    Break,
    Catch,
    Continue,
    Define,
    /// `.` representing the cursor.
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
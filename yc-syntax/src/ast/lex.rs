use lexical::format::GO_LITERAL;

use crate::ast::{char_stream::CharStream, Error, Position, Span};
use core::fmt;
use std::{cmp::Ordering, fmt::Display};

const LEFT_COMMENT_MARKER: &'static str = "/*";
const RIGHT_COMMENT_MARKER: &'static str = "*/";

const LEFT_ACTION_DELIM: &'static str = "{{";
const RIGHT_ACTION_DELIM: &'static str = "}}";

const LEFT_TRIM_MARKER: &'static str = "- ";
const RIGHT_TRIM_MARKER: &'static str = " -";

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

    Assign, // '=' introducing an assignment
    Comma,
    Comment(String), // value is the comment text, not including the delimiters
    Declare,         // ':=' introducing a declaration
    Field(String),   // alphanumeric identifier starting with '.'; value does not include the '.'
    Ident(String),   // alphanumeric identifier not starting with '.'
    LeftActionDelim { has_trim_marker: bool },
    LeftParen,
    Pipe,
    RawString(String), // value is the string content, not including the backticks
    RightActionDelim { has_trim_marker: bool },
    RightParen,
    String(String), // value is the interpreted string content, not including the quotation marks
    Text(String),   // plain text
    Variable(String), // value is the variable name, not including the $
    Whitespace(String), // run of spaces separating arguments

    Block,
    Break,
    Catch,
    Continue,
    Define,
    Dot, // '.' representing the cursor
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

#[derive(Debug)]
enum LexContext {
    Action,
    Comment,
    Text,
}

impl Default for LexContext {
    fn default() -> Self {
        Self::Text
    }
}

pub struct Lexer<'a> {
    source: CharStream<'a>,
    ctx: LexContext,
    last_token_end: Position,
    errors: Vec<Error>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: CharStream::new(source),
            ctx: LexContext::default(),
            last_token_end: Position::default(),
            errors: vec![],
        }
    }

    fn finish(self) -> Vec<Error> {
        self.errors
    }

    fn next_token(&mut self) -> Token {
        if self.source.at_eof() {
            self.emit(TokenKind::Eof)
        } else if self.at_start_of_action() {
            self.lex_start_of_action()
        } else {
            match self.ctx {
                LexContext::Action => self.lex_in_action(),
                LexContext::Comment => self.lex_comment(),
                LexContext::Text => self.lex_text(),
            }
        }
    }

    fn lex_text(&mut self) -> Token {
        let text = self.source.consume_until(LEFT_ACTION_DELIM);
        self.emit(TokenKind::Text(text))
    }

    fn lex_in_action(&mut self) -> Token {
        if self.at_end_of_action() {
            return self.lex_end_of_action();
        }

        match self.source.peek().unwrap() {
            ' ' | '\t' | '\r' | '\n' => self.lex_whitespace(),
            '=' => {
                self.source.skip(1);
                self.emit(TokenKind::Assign)
            }
            ':' => {
                let start = self.source.pos;
                self.source.skip(1);
                if self.source.accept('=') {
                    self.emit(TokenKind::Declare)
                } else {
                    self.error_at(self.source.span_after(start), "expected declaration");
                    self.emit(TokenKind::Invalid)
                }
            }
            '|' => {
                self.source.skip(1);
                self.emit(TokenKind::Pipe)
            }
            '"' => self.lex_string(),
            '`' => self.lex_raw_string(),
            '$' => self.lex_variable(),
            '\'' => self.lex_char(),
            '.' => {
                let followed_by_digit = self
                    .source
                    .peek_nth(1)
                    .filter(|c| c.is_ascii_digit())
                    .is_some();
                if followed_by_digit {
                    self.lex_numeric_literal()
                } else {
                    self.lex_field()
                }
            }
            '+' | '-' | '0'..='9' => self.lex_numeric_literal(),
            '(' => {
                self.source.skip(1);
                self.emit(TokenKind::LeftParen)
            }
            ')' => {
                self.source.skip(1);
                self.emit(TokenKind::RightParen)
            }
            ',' => {
                self.source.skip(1);
                self.emit(TokenKind::Comma)
            }
            c if c == '_' || c.is_alphanumeric() => self.lex_ident(),
            _ => {
                self.source.skip(1);
                self.error_at(self.source.span(), "unrecognized character in action");
                self.emit(TokenKind::Invalid)
            }
        }
    }

    fn at_start_of_action(&mut self) -> bool {
        self.source.lookahead(LEFT_ACTION_DELIM)
    }

    fn lex_start_of_action(&mut self) -> Token {
        self.source.must_consume(LEFT_ACTION_DELIM);
        let has_trim_marker = self.source.accept(LEFT_TRIM_MARKER);
        self.ctx = if self.source.lookahead(LEFT_COMMENT_MARKER) {
            LexContext::Comment
        } else {
            LexContext::Action
        };
        self.emit(TokenKind::LeftActionDelim { has_trim_marker })
    }

    fn at_end_of_action(&mut self) -> bool {
        self.source.lookahead(RIGHT_ACTION_DELIM) || self.source.lookahead(RIGHT_TRIM_MARKER)
    }

    fn lex_end_of_action(&mut self) -> Token {
        let start = self.source.pos;
        let has_trim_marker = self.source.accept(RIGHT_TRIM_MARKER);
        if has_trim_marker && !self.source.lookahead(RIGHT_ACTION_DELIM) {
            self.error_at(
                self.source.span_after(start),
                "expected right action delimiter to appear immediately after trim marker",
            );
            self.emit(TokenKind::Invalid)
        } else {
            self.source.skip(RIGHT_ACTION_DELIM.len());
            self.ctx = LexContext::Text;
            self.emit(TokenKind::RightActionDelim { has_trim_marker })
        }
    }

    fn lex_comment(&mut self) -> Token {
        let start = self.source.pos;
        self.source.must_consume(LEFT_COMMENT_MARKER);

        let comment_text = self.source.consume_until(RIGHT_COMMENT_MARKER);
        if self.source.accept(RIGHT_COMMENT_MARKER) {
            self.ctx = LexContext::Action;
            if !self.at_end_of_action() {
                self.error_at(
                    self.source.span_after(start),
                    "comment ends before closing delimiter",
                );
            }
        } else {
            self.error_at(self.source.span_after(start), "unclosed comment");
        }

        self.emit(TokenKind::Comment(comment_text))
    }

    fn lex_whitespace(&mut self) -> Token {
        let whitespace = self
            .source
            .consume_while(|c| matches!(c, ' ' | '\t' | '\r' | '\n'));
        self.emit(TokenKind::Whitespace(whitespace))
    }

    fn lex_ident(&mut self) -> Token {
        let ident = self.read_ident();
        self.expect_terminator();

        use TokenKind::*;
        self.emit(match ident.as_str() {
            "block" => Block,
            "break" => Break,
            "catch" => Catch,
            "continue" => Continue,
            "define" => Define,
            "else" => Else,
            "end" => End,
            "if" => If,
            "nil" => Nil,
            "range" => Range,
            "return" => Return,
            "template" => Template,
            "try" => Try,
            "while" => While,
            "with" => With,
            "true" => BoolLiteral(true),
            "false" => BoolLiteral(false),
            _ => Ident(ident),
        })
    }

    fn lex_variable(&mut self) -> Token {
        self.source.must_consume('$');
        let var_name = self.read_ident();
        self.expect_terminator();
        self.emit(TokenKind::Variable(var_name))
    }

    fn lex_field(&mut self) -> Token {
        self.source.must_consume('.');
        let field_name = self.read_ident();
        self.expect_terminator();
        self.emit(if field_name.is_empty() {
            TokenKind::Dot
        } else {
            TokenKind::Field(field_name)
        })
    }

    fn read_ident(&mut self) -> String {
        self.source
            .consume_while(|c| c == '_' || c.is_alphanumeric())
    }

    fn expect_terminator(&mut self) {
        // Note that if RIGHT_ACTION_DELIM is modified, the } here must be
        // changed appropriately as well.
        if !matches!(
            self.source.peek(),
            Some(' ' | '\t' | '\r' | '\n' | '.' | ',' | ':' | ')' | '(' | '}') | None
        ) {
            self.source.skip(1);
            self.error_at(self.source.span(), "expected terminator");
        }
    }

    fn lex_string(&mut self) -> Token {
        let start = self.source.pos;
        self.source.must_consume('"');
        let mut content = String::new();
        while !self.source.at_eof() && !self.source.accept('"') {
            if let Ok(c) = self.read_char('"') {
                content.push(c);
            }
        }

        if self.source.at_eof() {
            self.error_at(self.source.span_after(start), "unterminated quoted string");
        }
        self.emit(TokenKind::String(content))
    }

    fn lex_char(&mut self) -> Token {
        let start = self.source.pos;
        self.source.must_consume('\'');
        let kind = self
            .read_char('\'')
            .map(|c| TokenKind::CharLiteral(c))
            .unwrap_or(TokenKind::Invalid);

        if !self.source.accept('\'') {
            self.error_at(
                self.source.span_after(start),
                "unterminated character literal",
            );
        }
        self.emit(kind)
    }

    fn read_char(&mut self, delim: char) -> Result<char, ()> {
        let start = self.source.pos;
        let first = self.source.next().unwrap();
        if first != '\\' {
            return if first == '\n' {
                self.error_at(self.source.span(), "unexpected newline in quoted string");
                Err(())
            } else {
                Ok(first)
            };
        }

        let escapee = self.source.next().ok_or_else(|| {
            self.error_at(
                self.source.span_after(start),
                "unterminated escape sequence",
            )
        })?;
        Ok(match escapee {
            _ if escapee == delim => delim,
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '0' => '\0',

            'x' => self.read_multi_digit_escape(start, 0, 2, NumberBase::Hex)?,
            'u' => self.read_multi_digit_escape(start, 0, 4, NumberBase::Hex)?,
            'U' => self.read_multi_digit_escape(start, 0, 8, NumberBase::Hex)?,
            '0'..='7' => self.read_multi_digit_escape(
                start,
                escapee.to_digit(8).unwrap(),
                2,
                NumberBase::Octal,
            )?,
            _ => return Err(self.error_at(self.source.span(), "unknown escape character")),
        })
    }

    fn read_multi_digit_escape(
        &mut self,
        start: Position,
        initial_value: u32,
        expected_digits: usize,
        base: NumberBase,
    ) -> Result<char, ()> {
        let digits: Vec<_> = self
            .source
            .remaining()
            .chars()
            .map_while(|c| c.to_digit(base as _))
            .collect(); // TODO: figure out a way to avoid the allocation
        self.source.skip(digits.len());
        match digits.len().cmp(&expected_digits) {
            Ordering::Less => Err(self.error_at(
                self.source.span_after(start),
                "too few digits in escape sequence",
            )),
            Ordering::Equal => {
                let val = digits
                    .iter()
                    .fold(initial_value, |acc, digit| &acc * base as u32 + digit);
                Ok(char::from_u32(val).ok_or_else(|| {
                    self.error_at(
                        self.source.span_after(start),
                        "escape sequence results in invalid character",
                    )
                })?)
            }
            Ordering::Greater => Err(self.error_at(
                self.source.span_after(start),
                "too many digits in escape sequence",
            )),
        }
    }

    fn lex_raw_string(&mut self) -> Token {
        let start = self.source.pos;
        self.source.must_consume('`');

        let content = self.source.consume_until('`');
        if !self.source.accept('`') {
            self.error_at(
                self.source.span_after(start),
                "unterminated raw quoted string",
            );
        }
        self.emit(TokenKind::RawString(content))
    }

    // TODO: support complex literals?
    fn lex_numeric_literal(&mut self) -> Token {
        let start = self.source.pos;
        self.source.accept(&['+', '-']);
        let base = if self.source.accept('0') {
            if self.source.accept(&['x', 'X']) {
                NumberBase::Hex
            } else if self.source.accept(&['o', 'O']) {
                NumberBase::Octal
            } else if self.source.accept(&['b', 'B']) {
                NumberBase::Binary
            } else {
                NumberBase::Decimal
            }
        } else {
            NumberBase::Decimal
        };

        self.skip_digits(base);
        let has_decimal_point = self.source.accept('.');
        if has_decimal_point {
            self.skip_digits(NumberBase::Decimal);
        };

        let has_exp = match base {
            NumberBase::Decimal => self.source.accept(&['e', 'E']),
            NumberBase::Hex => self.source.accept(&['p', 'P']),
            _ => false,
        };
        if has_exp {
            self.source.accept(&['+', '-']);
            self.skip_digits(NumberBase::Decimal);
        };

        let span = self.source.span_after(start);
        let text = &self.source.text[span.range()];

        // TODO: emit more specific errors?
        if has_decimal_point || has_exp {
            let options = lexical::ParseFloatOptions::default();
            lexical::parse_with_options::<f64, _, GO_LITERAL>(text, &options)
                .map(|val| self.emit(TokenKind::FloatLiteral(val)))
                .unwrap_or_else(|_| {
                    self.error_at(span, "invalid number syntax");
                    self.emit(TokenKind::Invalid)
                })
        } else {
            lexical::parse::<i64, _>(text)
                .map(|val| self.emit(TokenKind::IntLiteral(val)))
                .unwrap_or_else(|_| {
                    self.error_at(span, "invalid number syntax");
                    self.emit(TokenKind::Invalid)
                })
        }
    }

    fn skip_digits(&mut self, base: NumberBase) {
        while self
            .source
            .peek()
            .filter(|c| c == &'_' || c.to_digit(base as _).is_some())
            .is_some()
        {
            self.source.skip(1);
        }
    }

    fn emit(&mut self, kind: TokenKind) -> Token {
        let token = Token::new(kind, self.source.span_after(self.last_token_end));
        self.last_token_end = self.source.pos;
        token
    }

    fn error_at<S>(&mut self, span: Span, message: S)
    where
        S: Into<String>,
    {
        self.errors.push(Error {
            message: message.into(),
            source_code: self.source.text.to_string(),
            span,
        });
    }
}

#[derive(Clone, Copy, Debug)]
enum NumberBase {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hex = 16,
}

impl Display for NumberBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use NumberBase::*;
        match self {
            Binary => write!(f, "binary"),
            Octal => write!(f, "octal"),
            Decimal => write!(f, "decimal"),
            Hex => write!(f, "hex"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter;
    use std::ops::Range;

    use crate::ast::lex::{Lexer, Token, TokenKind};
    use crate::ast::{Position, Span};
    use pretty_assertions::assert_eq;
    use TokenKind::*;

    fn assert_lex_ok(text: &str, want: Vec<Token>) {
        let mut lexer = Lexer::new(text);
        let got: Vec<_> = iter::from_fn(|| Some(lexer.next_token()))
            .take_while(|tok| tok.kind != TokenKind::Eof)
            .collect();
        assert_eq!(want, got);
        assert!(lexer.finish().is_empty());
    }

    fn tok(kind: TokenKind, span: Span) -> Token {
        Token::new(kind, span)
    }

    // Only works with ASCII source text with no newlines.
    fn span(range: Range<usize>) -> Span {
        Span::new(
            Position::new(range.start, 1, range.start + 1),
            Position::new(range.end, 1, range.end + 1),
        )
    }

    fn span_in(source: &str, range: Range<usize>) -> Span {
        fn resolve_pos(text: &str, offset: usize) -> Position {
            text[..offset]
                .chars()
                .fold(Position::default(), |pos, c| pos.advance_by(c))
        }

        Span::new(
            resolve_pos(source, range.start),
            resolve_pos(source, range.end),
        )
    }

    fn left_delim(has_trim_marker: bool, span: Span) -> Token {
        Token::new(LeftActionDelim { has_trim_marker }, span)
    }

    fn right_delim(has_trim_marker: bool, span: Span) -> Token {
        Token::new(RightActionDelim { has_trim_marker }, span)
    }

    #[test]
    fn empty() {
        assert_lex_ok("", vec![]);
    }

    #[test]
    fn spaces_in_text() {
        let source = " \t\n";
        assert_lex_ok(
            source,
            vec![tok(Text(" \t\n".to_string()), span_in(source, 0..3))],
        );
    }

    #[test]
    fn text() {
        assert_lex_ok(
            "now is the time",
            vec![tok(Text("now is the time".to_string()), span(0..15))],
        );
    }

    #[test]
    fn text_with_comment() {
        assert_lex_ok(
            "hello-{{/* this is a comment */}}-world",
            vec![
                tok(Text("hello-".to_string()), span(0..6)),
                left_delim(false, span(6..8)),
                tok(Comment(" this is a comment ".to_string()), span(8..31)),
                right_delim(false, span(31..33)),
                tok(Text("-world".to_string()), span(33..39)),
            ],
        );
    }

    #[test]
    fn empty_action() {
        assert_lex_ok(
            "{{}}",
            vec![
                left_delim(false, span(0..2)),
                right_delim(false, span(2..4)),
            ],
        );
    }

    #[test]
    fn identifier() {
        assert_lex_ok(
            "{{foo}}",
            vec![
                left_delim(false, span(0..2)),
                tok(Ident("foo".to_string()), span(2..5)),
                right_delim(false, span(5..7)),
            ],
        );
    }

    #[test]
    fn parens() {
        assert_lex_ok(
            "{{((((bar))))}}",
            vec![
                left_delim(false, span(0..2)),
                tok(LeftParen, span(2..3)),
                tok(LeftParen, span(3..4)),
                tok(LeftParen, span(4..5)),
                tok(LeftParen, span(5..6)),
                tok(Ident("bar".to_string()), span(6..9)),
                tok(RightParen, span(9..10)),
                tok(RightParen, span(10..11)),
                tok(RightParen, span(11..12)),
                tok(RightParen, span(12..13)),
                right_delim(false, span(13..15)),
            ],
        );
    }

    #[test]
    fn block() {
        assert_lex_ok(
            r#"{{block "foo" .}}"#,
            vec![
                left_delim(false, span(0..2)),
                tok(Block, span(2..7)),
                tok(Whitespace(" ".to_string()), span(7..8)),
                tok(String("foo".to_string()), span(8..13)),
                tok(Whitespace(" ".to_string()), span(13..14)),
                tok(Dot, span(14..15)),
                right_delim(false, span(15..17)),
            ],
        );
    }

    #[test]
    fn interpreted_string() {
        assert_lex_ok(
            r#"{{"abc \n\t\" "}}"#,
            vec![
                left_delim(false, span(0..2)),
                tok(String("abc \n\t\" ".to_string()), span(2..15)),
                right_delim(false, span(15..17)),
            ],
        );
    }

    #[test]
    fn raw_string() {
        assert_lex_ok(
            r#"{{`abc\n\t\" "`}}"#,
            vec![
                left_delim(false, span(0..2)),
                tok(RawString(r#"abc\n\t\" ""#.to_string()), span(2..15)),
                right_delim(false, span(15..17)),
            ],
        );
    }

    #[test]
    fn raw_string_with_newline() {
        // \n is interpreted literally in a raw string.
        assert_lex_ok(
            r#"{{`now is{{\n}}the time`}}"#,
            vec![
                left_delim(false, span(0..2)),
                tok(
                    RawString(r#"now is{{\n}}the time"#.to_string()),
                    span(2..24),
                ),
                right_delim(false, span(24..26)),
            ],
        );
    }

    // TODO: expand test suite
}

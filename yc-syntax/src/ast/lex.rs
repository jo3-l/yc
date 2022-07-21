use lexical::format::GO_LITERAL;

use crate::ast::{char_stream::CharStream, Error, Position, Span};
use core::fmt;
use std::{fmt::Display, result};

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
    BoolLiteral(bool), // boolean literal
    CharLiteral(char), // character literal
    FloatLiteral(f64), // floating point literal
    IntLiteral(i64),   // integer literal

    Assign,                                     // equals ('=') introducing an assignment
    Comma,                                      // comma (','), used in range action
    Comment(String),                            // comment text
    Declare,                                    // colon-equals (':=') introducing a declaration
    Field(String),                              // alphanumeric identifier starting with '.'
    Identifier(String),                         // alphanumeric identifier not starting with '.'
    LeftActionDelim { has_trim_marker: bool },  // left action delimiter
    LeftParen,                                  // '(' inside action
    Pipe,                                       // pipe symbol ('|')
    RawString(String),                          // raw quoted string
    RightActionDelim { has_trim_marker: bool }, // right action delimiter
    RightParen,                                 // '(' inside action
    String(String),                             // quoted string
    Text(String),                               // plain text
    Variable(String),                           // variable starting with '$'
    Whitespace(String),                         // run of spaces separating arguments

    Block,    // block keyword
    Break,    // break keyword
    Catch,    // catch keyword
    Continue, // continue keyword
    Define,   // define keyword
    Dot,      // the cursor ('.')
    Else,     // else keyword
    End,      // end keyword
    If,       // if keyword
    Nil,      // untyped nil constant
    Range,    // range keyword
    Return,   // return keyword
    Template, // template keyword
    Try,      // try keyword
    While,    // while keyword
    With,     // with keyword
}

pub struct Lexer<'a> {
    source: CharStream<'a>,
    state: State,
    last_token_end: Position,
}

#[derive(Debug)]
enum State {
    InAction,
    InComment,
    InText,
}

type Result<T = Token> = result::Result<T, Error>;

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: CharStream::new(source),
            state: State::InText,
            last_token_end: Position::default(),
        }
    }

    fn lex_text(&mut self) -> Result {
        let text = self.source.consume_until(LEFT_ACTION_DELIM);
        Ok(self.emit(TokenKind::Text(text)))
    }

    fn lex_in_action(&mut self) -> Result {
        if self.at_end_of_action() {
            return self.lex_end_of_action();
        };

        match self.source.peek().unwrap() {
            ' ' | '\t' | '\r' | '\n' => self.lex_whitespace(),
            '=' => self.lex_one(TokenKind::Assign),
            ':' => {
                let start = self.source.pos;
                self.source.skip(1);
                if self.source.accept('"') {
                    Ok(self.emit(TokenKind::Declare))
                } else {
                    Err(self.error(
                        self.source.span_after(start),
                        "comment ends before closing delimiter".to_string(),
                    ))
                }
            }
            '|' => self.lex_one(TokenKind::Pipe),
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
            '(' => self.lex_one(TokenKind::LeftParen),
            ')' => self.lex_one(TokenKind::RightParen),
            ',' => self.lex_one(TokenKind::Comma),
            c if c == '_' || c.is_alphanumeric() => self.lex_identifier(),
            _ => {
                self.source.skip(1);
                Err(self.error(
                    self.source.span(),
                    "unrecognized character in action".to_string(),
                ))
            }
        }
    }

    fn lex_one(&mut self, kind: TokenKind) -> Result {
        self.source.skip(1);
        Ok(self.emit(kind))
    }

    fn at_start_of_action(&mut self) -> bool {
        self.source.lookahead(LEFT_ACTION_DELIM)
    }

    fn lex_start_of_action(&mut self) -> Result {
        debug_assert!(self.source.lookahead(LEFT_ACTION_DELIM));

        self.source.skip(LEFT_ACTION_DELIM.len());
        let has_trim_marker = self.source.accept(LEFT_TRIM_MARKER);
        self.state = if self.source.lookahead(LEFT_COMMENT_MARKER) {
            State::InComment
        } else {
            State::InAction
        };
        Ok(self.emit(TokenKind::LeftActionDelim { has_trim_marker }))
    }

    fn at_end_of_action(&mut self) -> bool {
        self.source.lookahead(RIGHT_ACTION_DELIM) || self.source.lookahead(RIGHT_TRIM_MARKER)
    }

    fn lex_end_of_action(&mut self) -> Result {
        let start = self.source.pos;
        let has_trim_marker = self.source.accept(RIGHT_TRIM_MARKER);
        if has_trim_marker && !self.source.lookahead(RIGHT_ACTION_DELIM) {
            return Err(self.error(
                self.source.span_after(start),
                "expected right action delimiter to appear immediately after trim marker"
                    .to_string(),
            ));
        }

        self.source.skip(RIGHT_ACTION_DELIM.len());
        self.state = State::InText;
        Ok(self.emit(TokenKind::RightActionDelim { has_trim_marker }))
    }

    fn lex_comment(&mut self) -> Result {
        debug_assert!(self.source.lookahead(LEFT_COMMENT_MARKER));

        let start = self.source.pos;
        self.source.skip(LEFT_COMMENT_MARKER.len());

        let comment_text = self.source.consume_until(RIGHT_COMMENT_MARKER);
        if self.source.at_eof() {
            return Err(self.error(
                self.source.span_after(start),
                "unclosed comment".to_string(),
            ));
        }

        self.source.skip(RIGHT_COMMENT_MARKER.len());
        if self.at_end_of_action() {
            self.state = State::InAction;
            Ok(self.emit(TokenKind::Comment(comment_text)))
        } else {
            Err(self.error(
                self.source.span_after(start),
                "comment ends before closing delimiter".to_string(),
            ))
        }
    }

    fn lex_whitespace(&mut self) -> Result {
        let whitespace = self.source.consume_while(|c| c == ' ' || c == '\n');
        Ok(self.emit(TokenKind::Whitespace(whitespace)))
    }

    fn lex_identifier(&mut self) -> Result {
        use TokenKind::*;

        let identifier = self.read_identifier_like();
        self.ensure_terminator()?;
        Ok(self.emit(match identifier.as_str() {
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

            "true" | "false" => BoolLiteral(identifier == "true"),
            _ => Identifier(identifier),
        }))
    }

    fn lex_variable(&mut self) -> Result {
        debug_assert!(self.source.lookahead('$'));

        self.source.skip(1);
        let var_name = self.read_identifier_like();
        self.ensure_terminator()?;
        Ok(self.emit(TokenKind::Variable(var_name)))
    }

    fn lex_field(&mut self) -> Result {
        debug_assert!(self.source.lookahead('.'));

        self.source.skip(1);
        let field_name = self.read_identifier_like();
        self.ensure_terminator()?;
        Ok(self.emit(if field_name.is_empty() {
            TokenKind::Dot
        } else {
            TokenKind::Field(field_name)
        }))
    }

    fn ensure_terminator(&mut self) -> Result<()> {
        match self.source.peek() {
            Some(' ' | '\t' | '\r' | '\n' | '.' | ',' | '|' | ':' | ')' | '(' | '}') => Ok(()),
            Some(_) => {
                self.source.skip(1);
                Err(self.error(
                    self.source.span(),
                    "expected terminating character".to_string(),
                ))
            }
            _ => Ok(()),
        }
    }

    fn read_identifier_like(&mut self) -> String {
        self.source
            .consume_while(|c| c == '_' || c.is_alphanumeric())
    }

    fn lex_string(&mut self) -> Result {
        debug_assert!(self.source.lookahead('"'));

        let start = self.source.pos;
        self.source.skip(1);
        let mut content = String::new();
        while !self.source.at_eof() && !self.source.accept('"') {
            content.push(self.read_char('"')?);
        }

        if self.source.at_eof() {
            Err(self.error(
                self.source.span_after(start),
                "unterminated quoted string".to_string(),
            ))
        } else {
            Ok(self.emit(TokenKind::String(content)))
        }
    }

    fn lex_char(&mut self) -> Result {
        debug_assert!(self.source.lookahead('\''));

        let start = self.source.pos;
        self.source.skip(1);
        let c = self.read_char('\'')?;

        if self.source.accept('\'') {
            Ok(self.emit(TokenKind::CharLiteral(c)))
        } else {
            Err(self.error(
                self.source.span_after(start),
                "unterminated character literal".to_string(),
            ))
        }
    }

    fn read_char(&mut self, delimiter: char) -> Result<char> {
        debug_assert!(!self.source.at_eof());

        let start = self.source.pos;
        let first = self.source.next().unwrap();
        if first != '\\' {
            return Ok(first);
        };

        let escapee = self.source.next().ok_or_else(|| {
            self.error(
                self.source.span_after(start),
                "unterminated escape".to_string(),
            )
        })?;
        Ok(match escapee {
            _ if escapee == delimiter => delimiter,
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

            _ => return Err(self.error(self.source.span(), "unknown escape character".to_string())),
        })
    }

    fn read_multi_digit_escape(
        &mut self,
        start: Position,
        initial_value: u32,
        expected_digits: i32,
        base: NumberBase,
    ) -> Result<char> {
        let mut val = initial_value;
        for _ in 0..expected_digits {
            let c = self.source.next().ok_or_else(|| {
                self.error(
                    self.source.span_after(start),
                    "unterminated escape".to_string(),
                )
            })?;
            let digit = c.to_digit(base as _).ok_or_else(|| {
                self.error(
                    self.source.span(),
                    format!("non-{base} digit in escape sequence"),
                )
            })?;
            val = (val * base as u32) | digit;
        }

        Ok(char::from_u32(val).ok_or_else(|| {
            self.error(
                self.source.span_after(start),
                "invalid escape sequence".to_string(),
            )
        })?)
    }

    fn lex_raw_string(&mut self) -> Result {
        debug_assert!(self.source.lookahead('`'));

        let start = self.source.pos;
        self.source.skip(1);

        let content = self.source.consume_until('`');
        if self.source.at_eof() {
            Err(self.error(
                self.source.span_after(start),
                "unterminated raw quoted string".to_string(),
            ))
        } else {
            self.source.skip(1);
            Ok(self.emit(TokenKind::RawString(content)))
        }
    }

    // TODO: support complex literals?
    fn lex_numeric_literal(&mut self) -> Result {
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
                .map_err(|_| self.error(span, "invalid number syntax".to_string()))
                .map(|val| self.emit(TokenKind::FloatLiteral(val)))
        } else {
            lexical::parse::<i64, _>(text)
                .map_err(|_| self.error(span, "invalid number syntax".to_string()))
                .map(|val| self.emit(TokenKind::IntLiteral(val)))
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

    fn error(&self, span: Span, message: String) -> Error {
        Error {
            message,
            source_code: self.source.text.to_string(),
            span,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = result::Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.source.at_eof() {
            None
        } else if self.at_start_of_action() {
            Some(self.lex_start_of_action())
        } else {
            Some(match self.state {
                State::InAction => self.lex_in_action(),
                State::InComment => self.lex_comment(),
                State::InText => self.lex_text(),
            })
        }
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
    use std::ops::Range;

    use crate::ast::lex::{Lexer, Token, TokenKind};
    use crate::ast::{Error, Position, Span};
    use pretty_assertions::assert_eq;
    use TokenKind::*;

    fn lex(text: &str) -> Result<Vec<Token>, Error> {
        Lexer::new(text).collect()
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
        assert_eq!(lex(""), Ok(vec![]))
    }

    #[test]
    fn spaces_in_text() {
        let source = " \t\n";
        assert_eq!(
            lex(source),
            Ok(vec![tok(Text(" \t\n".to_string()), span_in(source, 0..3))])
        )
    }

    #[test]
    fn text() {
        assert_eq!(
            lex("now is the time"),
            Ok(vec![tok(Text("now is the time".to_string()), span(0..15))])
        )
    }

    #[test]
    fn text_with_comment() {
        assert_eq!(
            lex("hello-{{/* this is a comment */}}-world"),
            Ok(vec![
                tok(Text("hello-".to_string()), span(0..6)),
                left_delim(false, span(6..8)),
                tok(Comment(" this is a comment ".to_string()), span(8..31)),
                right_delim(false, span(31..33)),
                tok(Text("-world".to_string()), span(33..39)),
            ])
        )
    }

    #[test]
    fn empty_action() {
        assert_eq!(
            lex("{{}}"),
            Ok(vec![
                left_delim(false, span(0..2)),
                right_delim(false, span(2..4)),
            ])
        )
    }

    #[test]
    fn identifier() {
        assert_eq!(
            lex("{{foo}}"),
            Ok(vec![
                left_delim(false, span(0..2)),
                tok(Identifier("foo".to_string()), span(2..5)),
                right_delim(false, span(5..7)),
            ])
        )
    }

    #[test]
    fn parens() {
        assert_eq!(
            lex("{{((((bar))))}}"),
            Ok(vec![
                left_delim(false, span(0..2)),
                tok(LeftParen, span(2..3)),
                tok(LeftParen, span(3..4)),
                tok(LeftParen, span(4..5)),
                tok(LeftParen, span(5..6)),
                tok(Identifier("bar".to_string()), span(6..9)),
                tok(RightParen, span(9..10)),
                tok(RightParen, span(10..11)),
                tok(RightParen, span(11..12)),
                tok(RightParen, span(12..13)),
                right_delim(false, span(13..15)),
            ])
        )
    }

    #[test]
    fn block() {
        assert_eq!(
            lex(r#"{{block "foo" .}}"#),
            Ok(vec![
                left_delim(false, span(0..2)),
                tok(Block, span(2..7)),
                tok(Whitespace(" ".to_string()), span(7..8)),
                tok(String("foo".to_string()), span(8..13)),
                tok(Whitespace(" ".to_string()), span(13..14)),
                tok(Dot, span(14..15)),
                right_delim(false, span(15..17)),
            ])
        )
    }

    #[test]
    fn interpreted_string() {
        assert_eq!(
            lex(r#"{{"abc \n\t\" "}}"#),
            Ok(vec![
                left_delim(false, span(0..2)),
                tok(String("abc \n\t\" ".to_string()), span(2..15)),
                right_delim(false, span(15..17)),
            ])
        )
    }

    #[test]
    fn raw_string() {
        assert_eq!(
            lex(r#"{{`abc\n\t\" "`}}"#),
            Ok(vec![
                left_delim(false, span(0..2)),
                tok(RawString(r#"abc\n\t\" ""#.to_string()), span(2..15)),
                right_delim(false, span(15..17)),
            ])
        )
    }

    #[test]
    fn raw_string_with_newline() {
        // \n is interpreted literally in a raw string.
        assert_eq!(
            lex(r#"{{`now is{{\n}}the time`}}"#),
            Ok(vec![
                left_delim(false, span(0..2)),
                tok(
                    RawString(r#"now is{{\n}}the time"#.to_string()),
                    span(2..24)
                ),
                right_delim(false, span(24..26)),
            ])
        )
    }

    // TODO: expand test suite
}

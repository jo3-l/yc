use core::fmt;
use std::cmp::Ordering;
use std::fmt::Display;

use indoc::indoc;
use lexical::format::GO_LITERAL;
use yc_ast::location::{BytePos, Span};
use yc_ast::token::{Token, TokenKind};
use yc_diagnostics::{Diagnostic, FileId};

use crate::char_stream::CharStream;

const LEFT_COMMENT_MARKER: &'static str = "/*";
const RIGHT_COMMENT_MARKER: &'static str = "*/";

const LEFT_ACTION_DELIM: &'static str = "{{";
const RIGHT_ACTION_DELIM: &'static str = "}}";

const LEFT_TRIM_MARKER: &'static str = "- ";
const RIGHT_TRIM_MARKER: &'static str = " -";

/// Different contexts in which tokens can be parsed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

/// An error-tolerant lexer for YAGPDB-flavored Go templates.
pub struct Lexer<'src> {
    file_id: FileId,
    input: CharStream<'src>,
    ctx: LexContext,
    last_token_end: BytePos,
    diagnostics: Vec<Diagnostic>,
}

impl<'src> Lexer<'src> {
    pub fn new(file_id: FileId, source: &'src str) -> Self {
        Self {
            file_id,
            input: CharStream::new(source),
            ctx: LexContext::default(),
            last_token_end: BytePos::from_usize(0),
            diagnostics: vec![],
        }
    }

    pub fn source(&self) -> &'src str {
        self.input.src
    }

    /// Finishes the lexer and returns the accumulated diagnostics.
    pub fn finish(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    /// Returns an iterator over the tokens following the current token.
    /// To align with [Lexer::next_token], when the end of text is reached,
    /// [TokenKind::Eof] will be yielded endlessly.
    pub fn tokens<'lx>(&'lx mut self) -> Tokens<'lx, 'src> {
        Tokens { lexer: self }
    }

    /// Scans the next token. If the end of text has been reached,
    /// [TokenKind::Eof] will be returned.
    pub fn next_token(&mut self) -> Token {
        if self.input.at_eof() {
            self.mk_tok(TokenKind::Eof)
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

    /// Lexes a piece of literal text.
    fn lex_text(&mut self) -> Token {
        let text = self.input.consume_until(LEFT_ACTION_DELIM);
        self.mk_tok(TokenKind::Text(text))
    }

    /// Lexes a production within an action.
    fn lex_in_action(&mut self) -> Token {
        if self.at_end_of_action() {
            return self.lex_end_of_action();
        }

        match self.input.peek().unwrap() {
            ' ' | '\t' | '\r' | '\n' => self.lex_whitespace(),
            '=' => {
                self.input.skip(1);
                self.mk_tok(TokenKind::Assign)
            }
            ':' => {
                let start = self.input.pos;
                self.input.skip(1);
                if self.input.accept('=') {
                    self.mk_tok(TokenKind::Declare)
                } else {
                    self.add_diagnostic(
                        Diagnostic::error(self.file_id, "expected declaration")
                            .primary_span(self.input.span_from(start)),
                    );
                    self.mk_tok(TokenKind::Invalid)
                }
            }
            '|' => {
                self.input.skip(1);
                self.mk_tok(TokenKind::Pipe)
            }
            '"' => self.lex_string(),
            '`' => self.lex_raw_string(),
            '$' => self.lex_variable(),
            '\'' => self.lex_char_lit(),
            '.' => {
                // A '.' can begin a field or a numeric literal. Look at the
                // next character to disambiguate.
                let followed_by_digit = self
                    .input
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
                self.input.skip(1);
                self.mk_tok(TokenKind::LeftParen)
            }
            ')' => {
                self.input.skip(1);
                self.mk_tok(TokenKind::RightParen)
            }
            ',' => {
                self.input.skip(1);
                self.mk_tok(TokenKind::Comma)
            }
            c if c == '_' || c.is_alphanumeric() => self.lex_ident(),
            _ => {
                self.add_diagnostic(
                    Diagnostic::error(self.file_id, "unrecognized character in action")
                        .primary_span(self.input.cur_span()),
                );
                self.input.skip(1);
                self.mk_tok(TokenKind::Invalid)
            }
        }
    }

    /// Indicates whether an action starts at the current position.
    fn at_start_of_action(&mut self) -> bool {
        self.input.lookahead(LEFT_ACTION_DELIM)
    }

    /// Lexes the start of an action. The left action delimiter is known to be
    /// present.
    fn lex_start_of_action(&mut self) -> Token {
        self.input.must_consume(LEFT_ACTION_DELIM);
        let has_trim_marker = self.input.accept(LEFT_TRIM_MARKER);
        self.ctx = if self.input.lookahead(LEFT_COMMENT_MARKER) {
            LexContext::Comment
        } else {
            LexContext::Action
        };
        self.mk_tok(TokenKind::LeftActionDelim(has_trim_marker))
    }

    /// Indicates whether an action ends at the current position.
    fn at_end_of_action(&mut self) -> bool {
        self.input.lookahead(RIGHT_ACTION_DELIM) || self.input.lookahead(RIGHT_TRIM_MARKER)
    }

    /// Lexes the end of an action. Either the right trim marker or
    /// the right action delimiter is known to be present.
    fn lex_end_of_action(&mut self) -> Token {
        let start_pos = self.input.pos;
        let has_trim_marker = self.input.accept(RIGHT_TRIM_MARKER);
        let has_right_delim = self.input.accept(RIGHT_ACTION_DELIM);
        if has_trim_marker && !has_right_delim {
            self.add_diagnostic(
                Diagnostic::error(
                    self.file_id,
                    "expected right action delimiter to appear immediately after trim marker",
                )
                .primary(
                    self.input.span_from(start_pos),
                    "expected a right action delimiter after this",
                ),
            );
            self.mk_tok(TokenKind::Invalid)
        } else {
            assert!(has_right_delim);
            self.ctx = LexContext::Text;
            self.mk_tok(TokenKind::RightActionDelim(has_trim_marker))
        }
    }

    /// Lexes a comment. The left comment marker is known to be present.
    fn lex_comment(&mut self) -> Token {
        let start_span = self.input.cur_span();
        self.input.must_consume(LEFT_COMMENT_MARKER);
        let comment_text = self.input.consume_until(RIGHT_COMMENT_MARKER);
        if self.input.accept(RIGHT_COMMENT_MARKER) {
            self.ctx = LexContext::Action;
            if !self.at_end_of_action() {
                self.add_diagnostic(
                    Diagnostic::error(self.file_id, "comment ends before closing delimiter")
                        .primary(self.input.prev_span().unwrap(), "...and ends here")
                        .secondary(start_span, "a comment starts here")
                        .footer_note("note: comments cannot appear inside other actions"),
                );
            }
        } else {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "unclosed comment")
                    .primary(self.input.cur_span(), "...but the file ends here")
                    .secondary(start_span, "a comment starts here")
                    .footer_note("help: use */ to close the comment"),
            );
        }

        self.mk_tok(TokenKind::Comment(comment_text))
    }

    /// Lexes a run of whitespace within an action.
    fn lex_whitespace(&mut self) -> Token {
        let whitespace = self
            .input
            .consume_while(|c| matches!(c, ' ' | '\t' | '\r' | '\n'));
        self.mk_tok(TokenKind::Whitespace(whitespace))
    }

    /// Lexes an identifier.
    fn lex_ident(&mut self) -> Token {
        let ident = self.read_ident();
        self.expect_terminator();

        use TokenKind::*;
        self.mk_tok(match ident.as_str() {
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

    /// Lexes a variable. The '$' is known to be present.
    fn lex_variable(&mut self) -> Token {
        self.input.must_consume('$');
        let var_name = self.read_ident();
        self.expect_terminator();
        self.mk_tok(TokenKind::Variable(var_name))
    }

    /// Lexes a field. The '.' is known to be present.
    fn lex_field(&mut self) -> Token {
        self.input.must_consume('.');
        let field_name = self.read_ident();
        self.expect_terminator();
        self.mk_tok(if field_name.is_empty() {
            TokenKind::Dot
        } else {
            TokenKind::Field(field_name)
        })
    }

    fn read_ident(&mut self) -> String {
        self.input
            .consume_while(|c| c == '_' || c.is_alphanumeric())
    }

    /// Emits a diagnostic if the next character isn't a terminator.
    fn expect_terminator(&mut self) {
        // Note that if `RIGHT_ACTION_DELIM` is modified, the } here must be
        // changed appropriately as well.
        if !matches!(
            self.input.peek(),
            Some(' ' | '\t' | '\r' | '\n' | '.' | ',' | ':' | ')' | '(' | '}') | None
        ) {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "expected terminating character")
                    .primary_span(self.input.cur_span()),
            );
            self.input.skip(1);
        }
    }

    /// Lexes a quoted string literal. The left quotation mark is known to be
    /// present.
    fn lex_string(&mut self) -> Token {
        let start_span = self.input.cur_span();
        self.input.must_consume('"');

        let mut content = String::new();
        while !self.input.at_eof() && !self.input.accept('"') {
            if let Ok(c) = self.read_char(ReadCharContext::QuotedString, start_span) {
                content.push(c);
            }
        }

        if self.input.at_eof() {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "unterminated quoted string")
                    .primary(self.input.cur_span(), "...but the file ends here")
                    .secondary(start_span, "the string starts here")
                    .footer_note(r#"help: use " to close the string"#),
            );
        }
        self.mk_tok(TokenKind::String(content))
    }

    /// Lexes a character literal. The left quotation mark is known to be
    /// present.
    fn lex_char_lit(&mut self) -> Token {
        let start_span = self.input.cur_span();
        self.input.must_consume('\'');
        let kind = self
            .read_char(ReadCharContext::CharLiteral, start_span)
            .map(|c| TokenKind::CharLiteral(c))
            .unwrap_or(TokenKind::Invalid);

        if !self.input.accept('\'') {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "unterminated character literal")
                    .primary(self.input.cur_span(), "...and should end here")
                    .secondary(start_span, "the character literal starts here")
                    .footer_note("help: use ' to close the character literal"),
            );
        }
        self.mk_tok(kind)
    }

    /// Reads a character within a character literal or a quoted string literal,
    /// which may be represented by an escape sequence.
    fn read_char(&mut self, ctx: ReadCharContext, open_delim_span: Span) -> Result<char, ()> {
        let (first, span_of_first) = self.input.next_with_span().unwrap();
        if first != '\\' {
            if first == '\n' {
                let mut err =
                    Diagnostic::error(self.file_id, format!("unexpected newline in {ctx}"))
                        .primary(span_of_first, "...and the newline appears here")
                        .secondary(open_delim_span, format!("the {ctx} starts here"));
                if ctx == ReadCharContext::QuotedString {
                    err = err.footer_note(indoc! {r#"
                        help: to create a newline, use the escape sequence `\n`
                            alternately, use a raw string literal, which allows newlines to be inserted directly
                    "#});
                }

                self.add_diagnostic(err);
            }
            return Ok(first);
        }

        let (escapee, span_of_escapee) = self.input.next_with_span().ok_or_else(|| {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "expected escape sequence")
                    .primary(self.input.cur_span(), "...but the file ends here")
                    .secondary(
                        span_of_first,
                        "the backslash here begins an escape sequence",
                    ),
            );
        })?;
        Ok(match escapee {
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '0' => '\0',

            'x' => self.read_multi_digit_escape(span_of_first.start, 0, 2, NumberBase::Hex)?,
            'u' => self.read_multi_digit_escape(span_of_first.start, 0, 4, NumberBase::Hex)?,
            'U' => self.read_multi_digit_escape(span_of_first.start, 0, 8, NumberBase::Hex)?,
            '0'..='7' => self.read_multi_digit_escape(
                span_of_first.start,
                escapee.to_digit(8).unwrap(),
                2,
                NumberBase::Octal,
            )?,
            _ if escapee == ctx.delim() => ctx.delim(),
            _ => {
                self.add_diagnostic(
                    Diagnostic::error(self.file_id, "unrecognized escape character")
                        .primary_span(span_of_escapee),
                );
                return Err(());
            }
        })
    }

    /// Reads a multi-digit character escape sequence within a character literal
    /// or a quoted string literal.
    fn read_multi_digit_escape(
        &mut self,
        start_pos: BytePos,
        initial_value: u32,
        expected_digits: usize,
        base: NumberBase,
    ) -> Result<char, ()> {
        let digits: Vec<_> = self
            .input
            .remaining()
            .chars()
            .map_while(|c| c.to_digit(base as _))
            .collect(); // TODO: Figure out a way to avoid the allocation.
        self.input.skip(digits.len());
        match digits.len().cmp(&expected_digits) {
            Ordering::Less => {
                self.add_diagnostic(
                    Diagnostic::error(self.file_id, "too few digits in escape sequence")
                        .primary_span(self.input.span_from(start_pos)),
                );
                Err(())
            }
            Ordering::Equal => {
                let val = digits
                    .iter()
                    .fold(initial_value, |acc, digit| &acc * base as u32 + digit);
                Ok(char::from_u32(val).ok_or_else(|| {
                    self.add_diagnostic(
                        Diagnostic::error(
                            self.file_id,
                            "escape sequence results in invalid character",
                        )
                        .primary_span(self.input.span_from(start_pos))
                        .footer_note("note: valid characters are in the range 0 to Ox10FFFF, inclusive, excluding surrogate code points (0xD800 to 0xDFFF)"),
                    );
                })?)
            }
            Ordering::Greater => {
                self.add_diagnostic(
                    Diagnostic::error(self.file_id, "too many digits in escape sequence")
                        .primary_span(self.input.span_from(start_pos)),
                );
                Err(())
            }
        }
    }

    /// Lexes a raw string literal. The opening backtick is known to be present.
    fn lex_raw_string(&mut self) -> Token {
        let start_span = self.input.cur_span();
        self.input.must_consume('`');
        let content = self.input.consume_until('`');
        if !self.input.accept('`') {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "unterminated raw quoted string")
                    .primary(self.input.cur_span(), "...but the file ends here")
                    .secondary(start_span, "the string starts here"),
            );
        }
        self.mk_tok(TokenKind::RawString(content))
    }

    /// Lexes a numeric literal.
    ///
    /// TODO: Emit more specific diagnostics.
    /// TODO: Support complex literals.
    fn lex_numeric_literal(&mut self) -> Token {
        let start_pos = self.input.pos;
        self.input.accept(&['+', '-']);
        let base = if self.input.accept('0') {
            if self.input.accept(&['x', 'X']) {
                NumberBase::Hex
            } else if self.input.accept(&['o', 'O']) {
                NumberBase::Octal
            } else if self.input.accept(&['b', 'B']) {
                NumberBase::Binary
            } else {
                NumberBase::Decimal
            }
        } else {
            NumberBase::Decimal
        };

        self.skip_digits(base);
        let has_decimal_point = self.input.accept('.');
        if has_decimal_point {
            self.skip_digits(NumberBase::Decimal);
        };

        let has_exp = match base {
            NumberBase::Decimal => self.input.accept(&['e', 'E']),
            NumberBase::Hex => self.input.accept(&['p', 'P']),
            _ => false,
        };
        if has_exp {
            self.input.accept(&['+', '-']);
            self.skip_digits(NumberBase::Decimal);
        };

        let span = self.input.span_from(start_pos);
        let text = &self.input.src[span.as_range()];
        if has_decimal_point || has_exp {
            let options = lexical::ParseFloatOptions::default();
            lexical::parse_with_options::<f64, _, GO_LITERAL>(text, &options)
                .map(|val| self.mk_tok(TokenKind::FloatLiteral(val)))
                .unwrap_or_else(|_| {
                    self.add_diagnostic(
                        Diagnostic::error(self.file_id, "invalid number syntax").primary_span(span),
                    );
                    self.mk_tok(TokenKind::Invalid)
                })
        } else {
            lexical::parse::<i64, _>(text)
                .map(|val| self.mk_tok(TokenKind::IntLiteral(val)))
                .unwrap_or_else(|_| {
                    self.add_diagnostic(
                        Diagnostic::error(self.file_id, "invalid number syntax").primary_span(span),
                    );
                    self.mk_tok(TokenKind::Invalid)
                })
        }
    }

    fn skip_digits(&mut self, base: NumberBase) {
        while self
            .input
            .peek()
            .filter(|c| c == &'_' || c.to_digit(base as _).is_some())
            .is_some()
        {
            self.input.skip(1);
        }
    }

    /// Creates a new token with the given kind spanning from the end of the
    /// previous token created to the current position.
    fn mk_tok(&mut self, kind: TokenKind) -> Token {
        let token = Token::new(kind, self.input.span_from(self.last_token_end));
        self.last_token_end = self.input.pos;
        token
    }

    fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }
}

/// An iterator that yields tokens from a lexer.
///
/// This struct is created by the [Lexer::tokens] method. See its documentation
/// for more.
pub struct Tokens<'lx, 'src> {
    lexer: &'lx mut Lexer<'src>,
}

impl Iterator for Tokens<'_, '_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.lexer.next_token())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReadCharContext {
    QuotedString,
    CharLiteral,
}

impl ReadCharContext {
    fn delim(self) -> char {
        use ReadCharContext::*;
        match self {
            QuotedString => '"',
            CharLiteral => '\'',
        }
    }
}

impl Display for ReadCharContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ReadCharContext::*;
        match self {
            QuotedString => write!(f, "quoted string"),
            CharLiteral => write!(f, "character literal"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::string::String;
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;

    use pretty_assertions::assert_eq;
    use quickcheck_macros::quickcheck;
    use yc_ast::location::Span;
    use yc_ast::token::Token;
    use yc_ast::token::TokenKind::{self, *};
    use yc_diagnostics::Diagnostic;

    use crate::lex::Lexer;

    fn lex(text: &str) -> (Vec<Token>, Vec<Diagnostic>) {
        let mut lexer = Lexer::new(0, text);
        let tokens = lexer
            .tokens()
            .take_while(|token| token.kind != Eof)
            .collect();
        (tokens, lexer.finish())
    }

    fn assert_lex_ok(text: &str, want: Vec<Token>) {
        let (tokens, errors) = lex(text);
        assert_eq!(tokens, want);
        assert!(errors.is_empty());
    }

    fn tok(kind: TokenKind, span: Span) -> Token {
        Token::new(kind, span)
    }

    #[quickcheck]
    fn lexer_is_lossless(src: String) -> bool {
        let (sender, receiver) = mpsc::channel();
        let cloned = src.clone();
        thread::spawn(move || {
            let (tokens, _) = lex(&cloned);
            sender.send(tokens).unwrap();
        });
        let tokens = receiver
            .recv_timeout(Duration::from_secs(5))
            .unwrap_or_else(|_| panic!("infinite recursion in lexer with input: {src}"));
        let reconstructed: String = tokens
            .iter()
            .map(|token| &src[token.span.as_range()])
            .collect();
        reconstructed == src
    }

    #[test]
    fn empty() {
        assert_lex_ok("", vec![]);
    }

    #[test]
    fn spaces_in_text() {
        assert_lex_ok(" \t\n", vec![tok(Text(" \t\n".to_string()), (0..3).into())]);
    }

    #[test]
    fn text() {
        assert_lex_ok(
            "now is the time",
            vec![tok(Text("now is the time".to_string()), (0..15).into())],
        );
    }

    #[test]
    fn text_with_comment() {
        assert_lex_ok(
            "hello-{{/* this is a comment */}}-world",
            vec![
                tok(Text("hello-".to_string()), (0..6).into()),
                tok(LeftActionDelim(false), (6..8).into()),
                tok(Comment(" this is a comment ".to_string()), (8..31).into()),
                tok(RightActionDelim(false), (31..33).into()),
                tok(Text("-world".to_string()), (33..39).into()),
            ],
        );
    }

    #[test]
    fn empty_action() {
        assert_lex_ok(
            "{{}}",
            vec![
                tok(LeftActionDelim(false), (0..2).into()),
                tok(RightActionDelim(false), (2..4).into()),
            ],
        );
    }

    #[test]
    fn identifier() {
        assert_lex_ok(
            "{{foo}}",
            vec![
                tok(LeftActionDelim(false), (0..2).into()),
                tok(Ident("foo".to_string()), (2..5).into()),
                tok(RightActionDelim(false), (5..7).into()),
            ],
        );
    }

    #[test]
    fn parens() {
        assert_lex_ok(
            "{{((((bar))))}}",
            vec![
                tok(LeftActionDelim(false), (0..2).into()),
                tok(LeftParen, (2..3).into()),
                tok(LeftParen, (3..4).into()),
                tok(LeftParen, (4..5).into()),
                tok(LeftParen, (5..6).into()),
                tok(Ident("bar".to_string()), (6..9).into()),
                tok(RightParen, (9..10).into()),
                tok(RightParen, (10..11).into()),
                tok(RightParen, (11..12).into()),
                tok(RightParen, (12..13).into()),
                tok(RightActionDelim(false), (13..15).into()),
            ],
        );
    }

    #[test]
    fn block() {
        assert_lex_ok(
            r#"{{block "foo" .}}"#,
            vec![
                tok(LeftActionDelim(false), (0..2).into()),
                tok(Block, (2..7).into()),
                tok(Whitespace(" ".to_string()), (7..8).into()),
                tok(String("foo".to_string()), (8..13).into()),
                tok(Whitespace(" ".to_string()), (13..14).into()),
                tok(Dot, (14..15).into()),
                tok(RightActionDelim(false), (15..17).into()),
            ],
        );
    }

    #[test]
    fn interpreted_string() {
        assert_lex_ok(
            r#"{{"abc \n\t\" "}}"#,
            vec![
                tok(LeftActionDelim(false), (0..2).into()),
                tok(String("abc \n\t\" ".to_string()), (2..15).into()),
                tok(RightActionDelim(false), (15..17).into()),
            ],
        );
    }

    #[test]
    fn raw_string() {
        assert_lex_ok(
            r#"{{`abc\n\t\" "`}}"#,
            vec![
                tok(LeftActionDelim(false), (0..2).into()),
                tok(RawString(r#"abc\n\t\" ""#.to_string()), (2..15).into()),
                tok(RightActionDelim(false), (15..17).into()),
            ],
        );
    }

    #[test]
    fn raw_string_with_newline() {
        // \n is interpreted literally in a raw string.
        assert_lex_ok(
            r#"{{`now is{{\n}}the time`}}"#,
            vec![
                tok(LeftActionDelim(false), (0..2).into()),
                tok(
                    RawString(r#"now is{{\n}}the time"#.to_string()),
                    (2..24).into(),
                ),
                tok(RightActionDelim(false), (24..26).into()),
            ],
        );
    }

    // TODO: expand test suite
}

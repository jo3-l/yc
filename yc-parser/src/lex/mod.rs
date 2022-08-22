mod input_cursor;

use yc_ast::location::BytePos;
use yc_ast::token::{Token, TokenKind};
use yc_diagnostics::{Diagnostic, FileId};

use crate::lex::input_cursor::InputCursor;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum LexContext {
    #[default]
    Text,
    Action,
}

/// An error-tolerant lexer for YAGPDB-flavored Go templates.
pub struct Lexer<'src> {
    file_id: FileId,
    cursor: InputCursor<'src>,
    ctx: LexContext,
    cur_tok_start: BytePos,
    diagnostics: Vec<Diagnostic>,
}

impl<'src> Lexer<'src> {
    pub fn new(file_id: FileId, source: &'src str) -> Self {
        Self {
            file_id,
            cursor: InputCursor::new(source),
            ctx: LexContext::default(),
            cur_tok_start: BytePos::from_usize(0),
            diagnostics: vec![],
        }
    }

    pub fn source(&self) -> &'src str {
        self.cursor.src
    }

    pub fn at_eof(&self) -> bool {
        self.cursor.at_eof()
    }

    /// Finishes the lexer and returns the accumulated diagnostics.
    pub fn finish(mut self) -> Vec<Diagnostic> {
        while !self.at_eof() {
            self.next_token();
        }
        self.diagnostics
    }

    /// Scans the next token. If the end of text has been reached,
    /// [TokenKind::Eof] will be returned.
    pub fn next_token(&mut self) -> Token {
        if self.at_eof() {
            self.emit(TokenKind::Eof)
        } else if self.eat("{{") {
            self.ctx = LexContext::Action;
            self.emit(TokenKind::LeftActionDelim)
        } else {
            match self.ctx {
                LexContext::Action => self.lex_in_action(),
                LexContext::Text => self.lex_text(),
            }
        }
    }

    /// Lexes a piece of literal text.
    fn lex_text(&mut self) -> Token {
        while !self.cursor.at_eof() && !self.at("{{") {
            self.cursor.bump();
        }
        self.emit(TokenKind::Text)
    }

    /// Lexes a production within an action.
    fn lex_in_action(&mut self) -> Token {
        match self.cursor.first().unwrap() {
            '/' => {
                if self.cursor.nth(1) == Some('*') {
                    self.lex_comment()
                } else {
                    self.cursor.bump();
                    self.add_diagnostic(
                        Diagnostic::error(self.file_id, "expected `*` after `/`")
                            .primary_span(self.cursor.span()),
                    );
                    self.emit(TokenKind::Invalid)
                }
            }
            ' ' => {
                if self.cursor.nth(1) == Some('-') {
                    self.cursor.bump_n(2);
                    self.emit(TokenKind::RightTrimMarker)
                } else {
                    self.lex_whitespace()
                }
            }
            '\t' | '\r' | '\n' => self.lex_whitespace(),
            '=' => self.bump_and_emit(TokenKind::Assign),
            ':' => {
                self.cursor.bump();
                if self.eat('=') {
                    self.emit(TokenKind::Declare)
                } else {
                    self.add_diagnostic(
                        Diagnostic::error(self.file_id, "expected `=` after `:`")
                            .primary_span(self.cursor.span()),
                    );
                    self.emit(TokenKind::Invalid)
                }
            }
            '|' => self.bump_and_emit(TokenKind::Pipe),
            '"' => self.lex_string(),
            '`' => self.lex_raw_string_lit(),
            '$' => self.lex_variable(),
            '\'' => self.lex_char_lit(),
            '.' => {
                let followed_by_digit = self.cursor.nth(1).filter(|c| c.is_ascii_digit()).is_some();
                if followed_by_digit {
                    self.lex_numeric_literal()
                } else {
                    self.bump_and_emit(TokenKind::Dot)
                }
            }
            '-' => {
                if self.cursor.nth(1) == Some(' ') {
                    self.cursor.bump_n(2);
                    self.emit(TokenKind::LeftTrimMarker)
                } else {
                    self.lex_numeric_literal()
                }
            }
            '+' | '0'..='9' => self.lex_numeric_literal(),
            '(' => self.bump_and_emit(TokenKind::LeftParen),
            ')' => self.bump_and_emit(TokenKind::RightParen),
            ',' => self.bump_and_emit(TokenKind::Comma),
            '}' => {
                self.cursor.bump();
                if self.eat('}') {
                    self.ctx = LexContext::Text;
                    self.emit(TokenKind::RightActionDelim)
                } else {
                    self.add_diagnostic(
                        Diagnostic::error(self.file_id, "expected `}` after `}`")
                            .primary_span(self.cursor.span()),
                    );
                    self.emit(TokenKind::Invalid)
                }
            }
            c if c == '_' || c.is_alphanumeric() => self.lex_ident(),
            _ => {
                self.add_diagnostic(
                    Diagnostic::error(self.file_id, "unrecognized character in action")
                        .primary_span(self.cursor.span()),
                );
                self.cursor.bump();
                self.emit(TokenKind::Invalid)
            }
        }
    }

    /// Bumps the cursor to the next position and returns the token passed in.
    fn bump_and_emit(&mut self, tok: TokenKind) -> Token {
        self.cursor.bump();
        self.emit(tok)
    }

    /// Lexes a comment. The left comment marker is known to be present.
    fn lex_comment(&mut self) -> Token {
        let start_span = self.cursor.span();
        assert!(self.eat("/*"));
        while !self.at_eof() && !self.at("*/") {
            self.cursor.bump();
        }

        if !self.eat("*/") {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "unclosed comment")
                    .primary(self.cursor.span(), "...but the file ends here")
                    .secondary(start_span, "the comment starts here")
                    .footer_note("help: use */ to close the comment"),
            );
        }

        self.ctx = LexContext::Action;
        self.emit(TokenKind::Comment)
    }

    /// Lexes a run of whitespace within an action.
    fn lex_whitespace(&mut self) -> Token {
        self.cursor
            .bump_while(|c| matches!(c, ' ' | '\t' | '\r' | '\n'));
        self.emit(TokenKind::Whitespace)
    }

    /// Lexes an identifier.
    fn lex_ident(&mut self) -> Token {
        let start_pos = self.cursor.pos;
        self.cursor.bump_while(|c| c.is_alphanumeric() || c == '_');

        use TokenKind::*;
        self.emit(
            match &self.source()[self.cursor.span_after(start_pos).as_range()] {
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
                "true" | "false" => BoolLit,
                _ => Ident,
            },
        )
    }

    /// Lexes a variable. The '$' is known to be present.
    fn lex_variable(&mut self) -> Token {
        assert!(self.eat('$'));
        self.cursor.bump_while(|c| c.is_alphanumeric() || c == '_');
        self.emit(TokenKind::Variable)
    }

    /// Lexes a quoted string literal. The left quotation mark is known to be
    /// present. Syntax validation is performed by the parser.
    fn lex_string(&mut self) -> Token {
        let start_span = self.cursor.span();
        assert!(self.eat('"'));

        while !self.at_eof() && !self.at('"') {
            if self.at('\\') {
                self.cursor.bump();
            }
            self.cursor.bump();
        }
        if !self.eat('"') {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "unterminated quoted string")
                    .primary(self.cursor.span(), "...but the file ends here")
                    .secondary(start_span, "the string starts here")
                    .footer_note(r#"help: use " to close the string"#),
            );
        }
        self.emit(TokenKind::QuotedStringLit)
    }

    /// Lexes a character literal. The left quotation mark is known to be
    /// present. Syntax validation is performed by the parser.
    fn lex_char_lit(&mut self) -> Token {
        let start_span = self.cursor.span();
        assert!(self.eat('\''));

        while !self.at_eof() && !self.at('\'') {
            if self.at('\\') {
                self.cursor.bump();
            }
            self.cursor.bump();
        }
        if !self.eat('\'') {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "unterminated character literal")
                    .primary(self.cursor.span(), "...and should end here")
                    .secondary(start_span, "the character literal starts here")
                    .footer_note("help: use ' to close the character literal"),
            );
        }
        self.emit(TokenKind::CharLit)
    }

    /// Lexes a raw string literal. The opening backtick is known to be present.
    fn lex_raw_string_lit(&mut self) -> Token {
        let start_span = self.cursor.span();
        assert!(self.eat('`'));

        self.cursor.bump_while(|c| c != '`');
        if !self.eat('`') {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "unterminated raw quoted string")
                    .primary(self.cursor.span(), "...but the file ends here")
                    .secondary(start_span, "the string starts here"),
            );
        }
        self.emit(TokenKind::RawStringLit)
    }

    /// Lexes a numeric literal.
    ///
    /// TODO: Emit more specific diagnostics.
    /// TODO: Support complex literals.
    fn lex_numeric_literal(&mut self) -> Token {
        self.eat(&['+', '-']);
        let base = if self.eat('0') {
            if self.eat(&['x', 'X']) {
                16
            } else if self.eat(&['o', 'O']) {
                8
            } else if self.eat(&['b', 'B']) {
                2
            } else {
                10
            }
        } else {
            10
        };

        self.cursor.bump_while(|c| c.to_digit(base).is_some());
        let has_decimal_point = self.eat('.');
        if has_decimal_point {
            self.cursor.bump_while(|c| c.to_digit(10).is_some());
        };

        let has_exp = match base {
            10 => self.eat(&['e', 'E']),
            16 => self.eat(&['p', 'P']),
            _ => false,
        };
        if has_exp {
            self.eat(&['+', '-']);
            self.cursor.bump_while(|c| c.to_digit(10).is_some());
        };

        self.emit(if has_decimal_point || has_exp {
            TokenKind::FloatLit
        } else {
            TokenKind::IntLit
        })
    }

    /// If the pattern matches at the current position, advances the input
    /// cursor and returns true; otherwise, returns false.
    fn eat(&mut self, pat: impl StrPattern<'src>) -> bool {
        if self.at(pat) {
            for _ in 0..pat.match_len() {
                self.cursor.bump();
            }
            true
        } else {
            false
        }
    }

    /// Indicates whether the pattern matches at the current position.
    fn at(&mut self, pat: impl StrPattern<'src>) -> bool {
        pat.is_prefix_of(self.cursor.remaining())
    }

    /// Creates a new token with the given kind spanning from the end of the
    /// previous token created to the current position.
    fn emit(&mut self, kind: TokenKind) -> Token {
        let token = Token::new(kind, self.cursor.span_after(self.cur_tok_start));
        self.cur_tok_start = self.cursor.pos;
        token
    }

    fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }
}

/// A string pattern. Similar to [std::str::pattern::Pattern], but usable on
/// stable Rust.
trait StrPattern<'a>: Sized + Copy {
    /// Indicates whether the pattern matches at the start of the haystack.
    fn is_prefix_of(self, haystack: &'a str) -> bool;

    /// Returns the number of characters matched by this pattern.
    fn match_len(self) -> usize;
}

impl<'a> StrPattern<'a> for &'a str {
    fn is_prefix_of(self, haystack: &'a str) -> bool {
        haystack.starts_with(self)
    }

    fn match_len(self) -> usize {
        self.chars().count()
    }
}

impl<'a> StrPattern<'a> for char {
    fn is_prefix_of(self, haystack: &'a str) -> bool {
        haystack.starts_with(self)
    }

    fn match_len(self) -> usize {
        1
    }
}

impl<'a, const N: usize> StrPattern<'a> for &'a [char; N] {
    fn is_prefix_of(self, haystack: &'a str) -> bool {
        haystack.starts_with(self)
    }

    fn match_len(self) -> usize {
        1
    }
}

#[cfg(test)]
mod tests {
    use std::string::String;
    use std::sync::mpsc;
    use std::time::Duration;
    use std::{iter, thread};

    use pretty_assertions::assert_eq;
    use quickcheck_macros::quickcheck;
    use yc_ast::token::{Token, TokenKind};
    use yc_diagnostics::Diagnostic;

    use crate::lex::Lexer;

    fn lex(text: &str) -> (Vec<Token>, Vec<Diagnostic>) {
        let mut lexer = Lexer::new(0, text);
        let tokens = iter::repeat_with(|| lexer.next_token())
            .take_while(|tok| tok.kind != TokenKind::Eof)
            .collect();
        (tokens, lexer.finish())
    }

    fn assert_lex_ok(text: &str, want: Vec<Token>) {
        let (tokens, errors) = lex(text);
        assert_eq!(tokens, want);
        assert!(errors.is_empty());
    }

    macro_rules! tok {
        ($kind:tt @ $start:tt..$end:tt) => {
            Token::new(TokenKind::$kind, ($start..$end).into())
        };
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
        assert_lex_ok(" \t\n", vec![tok!(Text @ 0..3)]);
    }

    #[test]
    fn text() {
        assert_lex_ok("now is the time", vec![tok!(Text @ 0..15)]);
    }

    #[test]
    fn text_with_comment() {
        assert_lex_ok(
            "hello-{{/* this is a comment */}}-world",
            vec![
                tok!(Text @ 0..6),
                tok!(LeftActionDelim @ 6..8),
                tok!(Comment @ 8..31),
                tok!(RightActionDelim @ 31..33),
                tok!(Text @ 33..39),
            ],
        );
    }

    #[test]
    fn empty_action() {
        assert_lex_ok(
            "{{}}",
            vec![tok!(LeftActionDelim @ 0..2), tok!(RightActionDelim @ 2..4)],
        );
    }

    #[test]
    fn identifier() {
        assert_lex_ok(
            "{{foo}}",
            vec![
                tok!(LeftActionDelim @ 0..2),
                tok!(Ident @ 2..5),
                tok!(RightActionDelim @ 5..7),
            ],
        );
    }

    #[test]
    fn parens() {
        assert_lex_ok(
            "{{((((bar))))}}",
            vec![
                tok!(LeftActionDelim @ 0..2),
                tok!(LeftParen @ 2..3),
                tok!(LeftParen @ 3..4),
                tok!(LeftParen @ 4..5),
                tok!(LeftParen @ 5..6),
                tok!(Ident @ 6..9),
                tok!(RightParen @ 9..10),
                tok!(RightParen @ 10..11),
                tok!(RightParen @ 11..12),
                tok!(RightParen @ 12..13),
                tok!(RightActionDelim @ 13..15),
            ],
        );
    }

    #[test]
    fn block() {
        assert_lex_ok(
            r#"{{block "foo" .}}"#,
            vec![
                tok!(LeftActionDelim @ 0..2),
                tok!(Block @ 2..7),
                tok!(Whitespace @ 7..8),
                tok!(QuotedStringLit @ 8..13),
                tok!(Whitespace @ 13..14),
                tok!(Dot @ 14..15),
                tok!(RightActionDelim @ 15..17),
            ],
        );
    }

    #[test]
    fn interpreted_string() {
        assert_lex_ok(
            r#"{{"abc \n\t\" "}}"#,
            vec![
                tok!(LeftActionDelim @ 0..2),
                tok!(QuotedStringLit @ 2..15),
                tok!(RightActionDelim @ 15..17),
            ],
        );
    }

    #[test]
    fn raw_string() {
        assert_lex_ok(
            r#"{{`abc\n\t\" "`}}"#,
            vec![
                tok!(LeftActionDelim @ 0..2),
                tok!(RawStringLit @ 2..15),
                tok!(RightActionDelim @ 15..17),
            ],
        );
    }

    #[test]
    fn raw_string_with_newline() {
        // \n is interpreted literally in a raw string.
        assert_lex_ok(
            r#"{{`now is{{\n}}the time`}}"#,
            vec![
                tok!(LeftActionDelim @ 0..2),
                tok!(RawStringLit @ 2..24),
                tok!(RightActionDelim @ 24..26),
            ],
        );
    }

    // TODO: expand test suite
}

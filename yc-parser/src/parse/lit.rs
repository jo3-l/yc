use std::fmt::Display;

use indoc::indoc;
use lexical::format::GO_LITERAL;
use yc_ast::ast;
use yc_ast::location::{BytePos, Span};
use yc_ast::parsed_fragment::ParsedFragment;
use yc_ast::token::{Token, TokenKind};
use yc_diagnostics::Diagnostic;

use crate::parse::Parser;

impl<'src> Parser<'src> {
    pub(crate) fn parse_nil_lit(&mut self) -> ast::NilLit {
        let tok = self.must_eat_skip_spaces(TokenKind::Nil);
        ast::NilLit {
            id: self.next_node_id(),
            span: tok.span,
        }
    }

    pub(crate) fn parse_bool_lit(&mut self) -> ast::BoolLit {
        let tok = self.must_eat_skip_spaces(TokenKind::BoolLit);
        let val = match &self.source()[tok.span.as_range()] {
            "true" => true,
            "false" => false,
            src => panic!("unexpected bool lit token value: {src}"),
        };

        ast::BoolLit {
            id: self.next_node_id(),
            span: tok.span,
            val,
        }
    }

    pub(crate) fn parse_raw_string_lit(&mut self) -> ast::StringLit {
        let tok = self.must_eat_skip_spaces(TokenKind::RawStringLit);
        let inner_start_pos = tok.span.start + 1; // Ignore the opening delimiter.
        let inner_end_pos = if self.source()[tok.span.as_range()].ends_with('`') {
            // Ignore the closing delimiter.
            tok.span.end - 1
        } else {
            // The closing delimiter is missing; lexer should already have
            // emitted a relevant diagnostic.
            tok.span.end
        };

        ast::StringLit {
            id: self.next_node_id(),
            span: tok.span,
            kind: ast::StringLitKind::Raw,
            val: self.source()[inner_start_pos.as_usize()..inner_end_pos.as_usize()].to_string(),
        }
    }

    pub(crate) fn parse_float_lit(&mut self) -> ParsedFragment<ast::FloatLit> {
        let tok = self.must_eat_skip_spaces(TokenKind::FloatLit);
        let parsed = lexical::parse_with_options::<f64, _, GO_LITERAL>(
            &self.source()[tok.span.as_range()],
            &lexical::ParseFloatOptions::default(),
        );
        match parsed {
            Ok(parsed) => ParsedFragment::Present(ast::FloatLit {
                id: self.next_node_id(),
                span: tok.span,
                val: parsed,
            }),
            Err(err) => {
                self.error_invalid_number_syntax(tok.span, err);
                ParsedFragment::Absent
            }
        }
    }

    pub(crate) fn parse_int_lit(&mut self) -> ParsedFragment<ast::IntLit> {
        let tok = self.must_eat_skip_spaces(TokenKind::IntLit);
        let parsed = lexical::parse::<i64, _>(&self.source()[tok.span.as_range()]);
        match parsed {
            Ok(parsed) => ParsedFragment::Present(ast::IntLit {
                id: self.next_node_id(),
                span: tok.span,
                val: parsed,
            }),
            Err(err) => {
                self.error_invalid_number_syntax(tok.span, err);
                ParsedFragment::Absent
            }
        }
    }

    fn error_invalid_number_syntax(&mut self, span: Span, err: lexical::Error) {
        use lexical::Error::*;
        let msg = match err {
            Overflow(_) => "numeric overflow",
            Underflow(_) => "numeric underflow",
            InvalidDigit(_) => "invalid digit in numeric literal",
            EmptyMantissa(_) => "invalid empty mantissa in numeric literal",
            EmptyExponent(_) => "invalid empty exponent in numeric literal",
            // TODO: Provide specific errors for more cases.
            _ => "invalid number syntax",
        };
        self.add_diagnostic(Diagnostic::error(self.file_id, msg).with_primary_span(span));
    }

    pub(crate) fn parse_char_lit(&mut self) -> ParsedFragment<ast::CharLit> {
        let tok = self.must_eat_skip_spaces(TokenKind::CharLit);
        let open_delim_span = Span::new(tok.span.start, tok.span.start + 1);

        // Ignore the opening delimiter.
        let inner_span = Span::new(open_delim_span.end, tok.span.end);
        let mut it = self.source()[inner_span.as_range()]
            .char_indices()
            // Adjust the offset so it corresponds to the position in the
            // original source code.
            .map(|(offset, c)| (inner_span.start + offset, c));

        self.read_char(&mut it, ReadCharContext::CharLit, open_delim_span)
            .ok()
            .map(|c| ast::CharLit {
                id: self.next_node_id(),
                span: tok.span,
                val: c,
            })
            .into()
    }

    pub(crate) fn parse_quoted_string_lit(&mut self) -> ast::StringLit {
        let tok = self.must_eat_skip_spaces(TokenKind::QuotedStringLit);
        let open_delim_span = Span::new(tok.span.start, tok.span.start + 1);

        let inner_span = Span::new(open_delim_span.end, tok.span.end);
        let mut it = self.source()[inner_span.as_range()]
            .char_indices()
            .map(|(offset, c)| (inner_span.start + offset, c));

        let mut parsed = String::new();
        while it.clone().next().filter(|(_, c)| c != &'\'').is_some() {
            if let Ok(c) =
                self.read_char(&mut it, ReadCharContext::QuotedStringLit, open_delim_span)
            {
                parsed.push(c);
            }
        }
        ast::StringLit {
            id: self.next_node_id(),
            span: tok.span,
            kind: ast::StringLitKind::Interpreted,
            val: parsed,
        }
    }

    fn read_char<I>(
        &mut self,
        input: &mut I,
        ctx: ReadCharContext,
        open_delim_span: Span,
    ) -> Result<char, ()>
    where
        I: Iterator<Item = (BytePos, char)> + Clone,
    {
        let (first_char_pos, first_char) = input.next().unwrap();
        let first_char_span = Span::new(first_char_pos, first_char_pos + first_char.len_utf8());
        if first_char != '\\' {
            if first_char == '\n' {
                let mut err =
                    Diagnostic::error(self.file_id, format!("unexpected newline in {ctx}"))
                        .with_primary_label(first_char_span, "...and the newline appears here")
                        .with_secondary_label(open_delim_span, format!("the {ctx} starts here"));
                if ctx == ReadCharContext::QuotedStringLit {
                    err = err.with_footer_note(indoc! {r#"
                        help: to create a newline, use the escape sequence `\n`
                            alternately, switch to a raw string literal and write newlines directly
                    "#});
                }
                self.add_diagnostic(err)
            }
            return Ok(first_char);
        }

        let mut clone = input.clone();
        // If there's no character after the backslash, then the string is
        // unterminated. Don't emit a diagnostic; the lexer should already have
        // done so.
        let (escaped_char_pos, escaped_char) = input.next().ok_or(())?;
        Ok(match escaped_char {
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '0' => '\0',

            'x' => self.read_multi_digit_escape(input, EscapeSequenceKind::Hex2, first_char_pos)?,
            'u' => self.read_multi_digit_escape(input, EscapeSequenceKind::Hex4, first_char_pos)?,
            'U' => self.read_multi_digit_escape(input, EscapeSequenceKind::Hex8, first_char_pos)?,
            '0'..='7' => {
                // The character just consumed was a digit; passing in `input`
                // here would cause it to be skipped. To avoid this, pass in a
                // clone of the iterator just before the digit was read.
                self.read_multi_digit_escape(&mut clone, EscapeSequenceKind::Octal, first_char_pos)?
            }
            _ if escaped_char == ctx.close_delim() => ctx.close_delim(),
            _ => {
                self.add_diagnostic(
                    Diagnostic::error(self.file_id, "unrecognized character in escape sequence")
                        .with_primary_span(Span::new(
                            escaped_char_pos,
                            escaped_char_pos + escaped_char.len_utf8(),
                        )),
                );
                return Err(());
            }
        })
    }

    fn read_multi_digit_escape<I>(
        &mut self,
        input: &mut I,
        kind: EscapeSequenceKind,
        backslash_pos: BytePos,
    ) -> Result<char, ()>
    where
        I: Iterator<Item = (BytePos, char)> + Clone,
    {
        // TODO: Figure out how to avoid the allocation.
        let digits: Vec<_> = input
            .clone()
            .take(kind.expected_digits())
            .map_while(|(offset, c)| c.to_digit(kind.digit_radix()).map(|digit| (offset, digit)))
            .collect();
        input.nth(digits.len()); // TODO: Switch to advance_by when stabilized.

        let end_pos = digits.last().map_or(backslash_pos, |(pos, _)| *pos + 1);
        if digits.len() < kind.expected_digits() {
            self.add_diagnostic(
                Diagnostic::error(self.file_id, "too few digits in escape sequence")
                    .with_primary_span(Span::new(backslash_pos, end_pos)),
            );
            return Err(());
        }

        let val = digits
            .iter()
            .fold(0u32, |acc, (_, digit)| &acc + kind.digit_radix() + digit);
        char::from_u32(val).ok_or_else(|| {
            self.add_diagnostic(Diagnostic::error(self.file_id, "escape sequence results in invalid character")
            .with_primary_span(Span::new(backslash_pos, end_pos))
            .with_footer_note("note: valid characters are in the range 0 to Ox10FFFF, inclusive, excluding surrogate code points (0xD800 to 0xDFFF)"));
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReadCharContext {
    CharLit,
    QuotedStringLit,
}

impl ReadCharContext {
    fn close_delim(&self) -> char {
        use ReadCharContext::*;
        match self {
            QuotedStringLit => '"',
            CharLit => '\'',
        }
    }
}

impl Display for ReadCharContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ReadCharContext::*;
        match self {
            CharLit => write!(f, "character literal"),
            QuotedStringLit => write!(f, "quoted string literal"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EscapeSequenceKind {
    Octal, // \nnn
    Hex2,  // \xhh
    Hex4,  // \uhhhh
    Hex8,  // \Uhhhhhhhh
}

impl EscapeSequenceKind {
    fn expected_digits(&self) -> usize {
        use EscapeSequenceKind::*;
        match self {
            Octal => 3,
            Hex2 => 2,
            Hex4 => 4,
            Hex8 => 8,
        }
    }

    fn digit_radix(&self) -> u32 {
        if *self == EscapeSequenceKind::Octal {
            8
        } else {
            16
        }
    }
}

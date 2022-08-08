use std::{
    borrow::Cow,
    collections::VecDeque,
    ops::{Range, RangeBounds},
};

use crate::{options::PrinterOptions, output_buffer::OutputBuffer};

#[derive(Debug)]
enum Token {
    ConnectedBreak { grouping_level: isize },
    Indent,
    Dedent,
    Space,
    Newline,
    Text(Cow<'static, str>),
}

#[derive(Debug)]
struct Break {
    grouping_level: isize,
    is_connected: bool,
    // Tokens located to the left of this line break.
    range: Range<usize>,
}

pub(crate) struct Printer {
    options: PrinterOptions,

    output: OutputBuffer,
    // Tokens waiting to be flushed to the output buffer.
    pending: VecDeque<Token>,
    // Possible conditional breaks to take. Invariant: rightmost element is the
    // outermost break available; choosing it will break the least number of
    // groupings.
    breaks: VecDeque<Break>,
    remaining_space: usize,
    // Deepest grouping level that is being broken across multiple lines.
    break_level: isize,
    current_level: isize,
}

// See "A New Language-Independent Prettyprinting Algorithm" by Pugh and
// Sinofsky for a description of the algorithm used.
impl Printer {
    pub(crate) fn new(options: &PrinterOptions) -> Self {
        Self {
            options: options.clone(),
            output: OutputBuffer::new(options.indent, options.tab_width),
            pending: VecDeque::new(),
            breaks: VecDeque::new(),
            remaining_space: options.max_width,
            break_level: -1,
            current_level: 0,
        }
    }

    pub(crate) fn finish(mut self) -> String {
        self.flush(..);
        self.output.to_string()
    }

    pub(crate) fn start_group(&mut self) {
        self.current_level += 1;
    }

    pub(crate) fn end_group(&mut self) {
        self.current_level -= 1;
        self.break_level = self.break_level.min(self.current_level);
    }

    pub(crate) fn print_space(&mut self) {
        self.pending.push_back(Token::Space);
    }

    pub(crate) fn print<S>(&mut self, text: S)
    where
        S: Into<Cow<'static, str>>,
    {
        let text = text.into();
        let len = text.len();
        if self.remaining_space < len {
            // Not enough space; split line here at the outermost break if
            // possible.
            if let Some(b) = self.breaks.pop_back() {
                self.break_level = b.grouping_level;
                self.flush(b.range);

                // Only print a line break if we're working with an optional
                // break. If we have a connected break, there should be a
                // corresponding token in the buffer that will force a line
                // break.
                if !b.is_connected {
                    self.output.write_newline();
                    self.remaining_space = self.options.max_width;
                }
                self.break_level = self.break_level.min(self.current_level);
            }

            // Do nothing if there are no breaks to take. Artificially inserting
            // a line break could result in undesirable output, like
            //   {{"long string"
            //   }}
        }

        self.pending.push_back(Token::Text(text));
        self.remaining_space = self.remaining_space.checked_sub(len).unwrap_or(0);
    }

    pub(crate) fn unconditional_break(&mut self) {
        self.breaks.clear();
        self.break_level = self.current_level;
        self.pending.push_back(Token::Newline);
        self.flush(..);
    }

    pub(crate) fn indent(&mut self) {
        self.pending.push_back(Token::Indent);
    }

    pub(crate) fn dedent(&mut self) {
        self.pending.push_back(Token::Dedent);
    }

    pub(crate) fn optional_break(&mut self) {
        let current_level = self.current_level;
        // Maintain the invariant that the rightmost break is the outermost one
        // (which will result in the least number of groupings being broken.)
        self.discard_breaks(|b| {
            b.grouping_level > current_level
                // Don't discard connected breaks at the same level; we might
                // need both.
                || (b.grouping_level == current_level && !b.is_connected)
        });

        self.breaks.push_back(Break {
            grouping_level: current_level,
            is_connected: false,
            range: 0..self.pending.len(),
        });
    }

    pub(crate) fn connected_break(&mut self) {
        if self.current_level < self.break_level {
            let current_level = self.current_level;
            // Maintain the invariant that the rightmost break is the outermost one
            // (which will result in the least number of groupings being broken.)
            self.discard_breaks(|b| b.grouping_level > current_level);

            self.pending.push_back(Token::ConnectedBreak {
                grouping_level: self.current_level,
            });
            self.breaks.push_back(Break {
                grouping_level: self.current_level,
                is_connected: true,
                range: 0..self.pending.len(),
            });
        } else {
            // at break level; split line here
            self.breaks.clear();
            self.pending.push_back(Token::Newline);
            self.flush(..);
        }
    }

    fn discard_breaks<F>(&mut self, mut predicate: F)
    where
        F: FnMut(&Break) -> bool,
    {
        while self.breaks.back().filter(|b| predicate(b)).is_some() {
            self.breaks.pop_back();
        }
    }

    fn flush<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        for token in self.pending.drain(range) {
            match token {
                Token::ConnectedBreak { grouping_level } => {
                    if grouping_level <= self.break_level {
                        self.output.write_newline();
                        self.remaining_space = self.options.max_width;
                    }
                }
                Token::Indent => self.output.indent(),
                Token::Dedent => self.output.dedent(),
                Token::Newline => {
                    self.output.write_newline();
                    self.remaining_space = self.options.max_width;
                }
                Token::Space => self.output.write_space(),
                Token::Text(text) => self.output.write(&text),
            }
        }
    }
}

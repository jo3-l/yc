use crate::options::IndentStyle;
use std::iter;

pub(crate) struct OutputBuffer {
    indent: IndentStyle,
    tab_width: usize,

    buf: String,
    // Indentation level of the next line of output.
    pending_indentation: usize,
    // Spaces to be outputted before the next token. Not written immediately to
    // avoid generating trailing whitespace.
    pending_spaces: usize,
}

impl OutputBuffer {
    pub(crate) fn new(indent: IndentStyle, tab_width: usize) -> Self {
        Self {
            indent,
            tab_width,
            buf: String::new(),
            pending_indentation: 0,
            pending_spaces: 0,
        }
    }

    pub(crate) fn finish(mut self) -> String {
        self.flush_spaces();
        self.buf
    }

    pub(crate) fn indent(&mut self) {
        self.pending_indentation += 1;
    }

    pub(crate) fn dedent(&mut self) {
        self.pending_indentation -= 1;
    }

    pub(crate) fn write(&mut self, text: &str) {
        self.flush_spaces();
        self.buf.push_str(text);
    }

    pub(crate) fn write_space(&mut self) {
        self.pending_spaces += 1;
    }

    pub(crate) fn write_newline(&mut self) {
        self.buf.push('\n');
        self.buf.extend(match self.indent {
            IndentStyle::Spaces => {
                iter::repeat(' ').take(self.tab_width * self.pending_indentation)
            }
            IndentStyle::Tabs => iter::repeat('\t').take(self.pending_indentation),
        });
        self.pending_spaces = 0;
    }

    fn flush_spaces(&mut self) {
        if self.pending_spaces > 0 {
            self.buf.extend(iter::repeat(' ').take(self.pending_spaces));
            self.pending_spaces = 0;
        }
    }
}

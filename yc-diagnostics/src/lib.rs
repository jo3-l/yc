use std::ops::Range;

pub use codespan_diagnostics::{LabelStyle, Severity};
use codespan_reporting::diagnostic as codespan_diagnostics;

pub type FileId = usize;

pub struct Diagnostic {
    pub file_id: FileId,
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<codespan_diagnostics::Label<FileId>>,
    pub notes: Vec<String>,
}

macro_rules! impl_constructor {
    ($name:ident, $severity:expr) => {
        impl Diagnostic {
            pub fn $name(file_id: FileId, message: impl Into<String>) -> Self {
                Self::new(file_id, $severity, message)
            }
        }
    };
}

impl_constructor!(error, Severity::Error);
impl_constructor!(warning, Severity::Warning);
impl_constructor!(note, Severity::Note);
impl_constructor!(help, Severity::Help);

impl Diagnostic {
    pub fn new(file_id: FileId, severity: Severity, message: impl Into<String>) -> Self {
        Self {
            file_id,
            severity,
            code: None,
            message: message.into(),
            labels: vec![],
            notes: vec![],
        }
    }

    pub fn code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn primary_span(self, span: impl Into<Range<usize>>) -> Self {
        self.label(LabelStyle::Primary, span, "")
    }

    pub fn primary(self, span: impl Into<Range<usize>>, message: impl Into<String>) -> Self {
        self.label(LabelStyle::Primary, span, message)
    }

    pub fn secondary(self, span: impl Into<Range<usize>>, message: impl Into<String>) -> Self {
        self.label(LabelStyle::Secondary, span, message)
    }

    pub fn label(
        mut self,
        style: LabelStyle,
        span: impl Into<Range<usize>>,
        message: impl Into<String>,
    ) -> Self {
        self.labels.push(codespan_diagnostics::Label {
            style,
            file_id: self.file_id,
            range: span.into(),
            message: message.into(),
        });
        self
    }

    pub fn footer_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn build(self) -> codespan_diagnostics::Diagnostic<FileId> {
        codespan_diagnostics::Diagnostic {
            severity: self.severity,
            code: self.code,
            message: self.message,
            labels: self.labels,
            notes: self.notes,
        }
    }
}

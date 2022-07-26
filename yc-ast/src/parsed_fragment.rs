//! Provides a container for missing AST nodes.
//!
//! # Comparison with `Option`
//!
//! `Option` and `ParsedFragment` both represent optional values; however, the
//! two types have different meanings within the context of the AST. An
//! `Option<Node>` indicates that the given node may be omitted in a well-formed
//! program. For example, the condition expression of an `else` branch could be
//! represented as an `Option<Node>`, as both `{{else if cond}}` and `{{else}}`
//! are valid syntactically.
//!
//! In contrast, a `ParsedFragment<Node>` indicates that the given node should
//! never be missing in a well-formed program, but can be missing in partial
//! ASTs representing invalid programs to facilitate error recovery.

/// A container for a potentially missing AST node.
///
/// See the module-level documentation for details.
#[derive(Clone, Debug)]
pub enum ParsedFragment<T> {
    /// The AST node is missing.
    Absent,
    /// The AST node is present.
    Present(T),
}

impl<T> ParsedFragment<T> {
    /// Converts from `ParsedFragment<T>` to `Option<T>`, consuming `self`.
    pub fn ok(self) -> Option<T> {
        match self {
            ParsedFragment::Absent => None,
            ParsedFragment::Present(x) => Some(x),
        }
    }

    /// Indicates whether the parsed fragment is absent.
    pub fn is_absent(&self) -> bool {
        matches!(self, ParsedFragment::Absent)
    }

    /// Indicates whether the parsed fragment is present.
    pub fn is_present(&self) -> bool {
        matches!(self, ParsedFragment::Present(_))
    }

    /// Returns the contained `Present` value, consuming the `self` value.
    ///
    /// Panics if the value is `Absent`.
    pub fn unwrap(self) -> T {
        match self {
            ParsedFragment::Absent => {
                panic!("called `ParsedFragment::unwrap()` on an `Absent` value")
            }
            ParsedFragment::Present(x) => x,
        }
    }

    /// Maps a `ParsedFragment<T>` to `ParsedFragment<U>` by applying a function
    /// to a contained value.
    pub fn map<U, F>(self, f: F) -> ParsedFragment<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            ParsedFragment::Absent => ParsedFragment::Absent,
            ParsedFragment::Present(x) => ParsedFragment::Present(f(x)),
        }
    }
}

impl<T> From<ParsedFragment<T>> for Option<T> {
    fn from(frag: ParsedFragment<T>) -> Self {
        frag.ok()
    }
}

impl<T> From<Option<T>> for ParsedFragment<T> {
    fn from(opt: Option<T>) -> Self {
        opt.map_or(ParsedFragment::Absent, ParsedFragment::Present)
    }
}

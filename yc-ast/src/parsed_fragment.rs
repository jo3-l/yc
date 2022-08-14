use std::fmt::Debug;

#[derive(Clone, Debug)]
pub enum ParsedFragment<T: Clone + Debug> {
    Absent,
    Present(T),
}

impl<T: Clone + Debug + PartialEq> ParsedFragment<T> {
    pub fn ok(self) -> Option<T> {
        match self {
            ParsedFragment::Absent => None,
            ParsedFragment::Present(value) => Some(value),
        }
    }

    pub fn is_absent(self) -> bool {
        matches!(self, ParsedFragment::Absent)
    }

    pub fn is_present(self) -> bool {
        matches!(self, ParsedFragment::Present(_))
    }

    pub fn unwrap(self) -> T {
        match self {
            ParsedFragment::Absent => {
                panic!("called `ParsedFragment::unwrap()` on an `Absent` value")
            }
            ParsedFragment::Present(value) => value,
        }
    }
}

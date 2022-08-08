#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PrinterOptions {
    pub(crate) max_width: usize,
    pub(crate) indent: IndentStyle,
    pub(crate) tab_width: usize,
    pub(crate) action_delim_spacing: bool,
}

impl Default for PrinterOptions {
    fn default() -> Self {
        Self {
            max_width: 120,
            indent: IndentStyle::Tabs,
            tab_width: 4,
            action_delim_spacing: false,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum IndentStyle {
    Spaces,
    Tabs,
}

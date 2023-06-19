use std::rc::Rc;

use crate::chunk::StringInterner;

#[derive(Debug, Clone, Eq)]
pub struct RloxString(pub Rc<str>);

impl RloxString {
    pub fn new(s: &str, interner: &impl StringInterner) -> Self {
        Self(interner.intern_string(s))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl PartialEq for RloxString {
    fn eq(&self, other: &Self) -> bool {
        // Strings are interned via reference counting, so comparing by pointer is enough.
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl std::fmt::Display for RloxString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

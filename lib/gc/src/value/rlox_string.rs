use crate::string_interner::{StrId, StringInterner};

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct RloxString(StrId);

impl RloxString {
    pub fn new(s: &str, interner: &mut StringInterner) -> Self {
        Self(interner.intern(s))
    }

    pub fn resolve<'a>(&self, interner: &'a StringInterner) -> &'a str {
        interner.lookup(self.0)
    }
}

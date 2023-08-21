use std::{hash::Hash, ops::Deref};

use crate::table::StringTable;

#[derive(Eq, Clone, Copy)]
pub struct InternedString {
    ptr: *const str,
    pub(crate) hash: usize,
}

impl InternedString {
    fn hash(s: &str) -> usize {
        s.bytes().fold(2166136261, |hash, byte| (hash ^ byte as usize).wrapping_mul(16777619))
    }

    pub fn new(s: &str) -> Self {
        Self { ptr: s as *const str, hash: Self::hash(s) }
    }

    #[allow(clippy::op_ref)]
    /// Compare actual underlying strings, instead of just comparing pointers.
    /// This is only needed when interning new strings.
    pub(crate) fn str_eq(&self, other: &InternedString) -> bool {
        unsafe { &(*self.ptr) == &(*other.ptr) }
    }
}

impl Hash for InternedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.ptr, state)
    }
}

impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

impl std::fmt::Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.deref())
    }
}

impl std::fmt::Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.debug_struct("InternedString")
                .field("ptr", &self.ptr)
                .field("hash", &self.hash)
                .field("string", &self.deref())
                .finish()
        } else {
            write!(f, "\"{}\"", self.deref())
        }
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        // SAFETY: We never remove interned strings, so the pointer is always valid.
        // We also only hand out immutable pointers, so dereferencing never violates Rust's
        // mutability rules.
        unsafe { &*self.ptr }
    }
}

#[derive(Debug)]
pub struct StringInterner {
    set: StringTable<()>,
    strings: Vec<String>,
}

impl StringInterner {
    pub fn with_capacity(cap: usize) -> StringInterner {
        StringInterner {
            set: StringTable::new(),
            strings: Vec::with_capacity(cap.next_power_of_two()),
        }
    }

    pub fn intern(&mut self, s: &str) -> InternedString {
        let hash = InternedString::hash(s);
        match self.set.get_str_eq(InternedString { ptr: s as *const str, hash }) {
            Some(interned_string) => *interned_string,
            None => {
                self.strings.push(s.to_string());

                let newly_interned_string =
                    InternedString { ptr: self.strings.last().unwrap().as_str(), hash };

                self.set.insert(newly_interned_string, ());

                newly_interned_string
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;

    #[test]
    fn intering() {
        let mut interner = StringInterner::with_capacity(1024);

        assert_eq!(interner.intern("a"), interner.intern("a"));
        assert_eq!(interner.intern("b"), interner.intern("b"));
        assert_eq!(interner.intern("ab"), interner.intern(&("a".to_string() + "b")));

        let s = "s".to_string();
        assert_eq!(interner.intern(&s), interner.intern(&s.clone()));
    }
}

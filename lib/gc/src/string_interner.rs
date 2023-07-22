use std::{
    collections::HashSet,
    hash::Hash,
    ops::Deref,
};

#[derive(Eq, Debug, Clone, Copy)]
pub struct InternedString(*const str);

impl Hash for InternedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state)
    }
}

impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl std::fmt::Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.deref())
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        // SAFETY: We never remove interned strings, so the pointer is always valid.
        // We also only hand out immutable pointers, so dereferencing never violates Rust's
        // mutability rules.
        unsafe { &*self.0 }
    }
}

#[derive(Debug)]
pub struct StringInterner {
    set: HashSet<&'static str>,
    strings: Vec<String>,
}

impl StringInterner {
    pub fn with_capacity(cap: usize) -> StringInterner {
        StringInterner {
            set: HashSet::default(),
            strings: Vec::with_capacity(cap.next_power_of_two()),
        }
    }

    pub fn intern(&mut self, name: &str) -> InternedString {
        if let Some(&id) = self.set.get(&name) {
            return InternedString(id);
        }

        self.strings.push(name.to_string());

        let ptr = self.strings.last().unwrap().as_str() as *const str;
        self.set.insert(unsafe { &*ptr });

        InternedString(ptr)
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

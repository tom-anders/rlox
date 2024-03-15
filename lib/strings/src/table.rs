use crate::string_interner::InternedString;
use std::{fmt::Debug, ops::Deref};

#[derive(Clone)]
pub struct StringTable<V> {
    entries: Vec<Option<Entry<V>>>,
    len: usize,
}

impl<V: Debug> Debug for StringTable<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<V: PartialEq> PartialEq for StringTable<V> {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        self.entries.iter().flatten().all(|entry| match other.get(entry.key) {
            None => false,
            Some(value) => entry.value().eq(value),
        })
    }
}

const MAX_LOAD: f32 = 0.75;

impl<V> StringTable<V> {
    pub fn with_capacity(cap: usize) -> Self {
        Self { entries: (0..cap).map(|_| None).collect(), len: 0 }
    }

    pub fn new() -> Self {
        Self::with_capacity(8)
    }

    fn check_capacity(&mut self) {
        if self.len() < (self.capacity() as f32 * MAX_LOAD) as usize {
            return;
        }

        let new_capacity = self.capacity() * 2;

        let mut new_entries = (0..new_capacity).map(|_| None).collect();

        std::mem::swap(&mut self.entries, &mut new_entries);

        for entry in new_entries.into_iter().flatten() {
            let dest = self.find_entry_mut(entry.key);
            *dest = Some(entry);
        }
    }

    pub fn insert(&mut self, key: InternedString, value: V) -> bool {
        self.check_capacity();

        let entry = self.find_entry_mut(key);
        let is_new_key = entry.is_none();

        *entry = Some(Entry { key, value });

        if is_new_key {
            self.len += 1;
        }

        is_new_key
    }

    pub fn get(&self, key: InternedString) -> Option<&V> {
        self.find_entry(key).as_ref().map(Entry::value)
    }

    pub(crate) fn get_str_eq(&self, key: InternedString) -> Option<&InternedString> {
        self.entries[self.find_entry_index(key, InternedString::str_eq)].as_ref().map(Entry::key)
    }

    pub fn get_mut(&mut self, key: InternedString) -> Option<&mut V> {
        self.find_entry_mut(key).as_mut().map(Entry::value_mut)
    }

    fn find_entry(&self, key: InternedString) -> &Option<Entry<V>> {
        &self.entries[self.find_entry_index(key, InternedString::eq)]
    }

    fn find_entry_mut(&mut self, key: InternedString) -> &mut Option<Entry<V>> {
        let index = self.find_entry_index(key, InternedString::eq);
        &mut self.entries[index]
    }

    fn find_entry_index(
        &self,
        key: InternedString,
        cmp: impl Fn(&InternedString, &InternedString) -> bool,
    ) -> usize {
        // capacity is always a power of 2, in which case this is
        // equivalent to key.hash % self.capacity(), but a lot faster.
        let mut first_index = key.hash & (self.capacity() - 1);

        loop {
            match &self.entries[first_index] {
                None => return first_index,
                Some(entry) => {
                    if cmp(&entry.key, &key) {
                        return first_index;
                    }
                }
            }
            first_index = (first_index + 1) & (self.capacity() - 1);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &V)> {
        self.entries.iter().flatten().map(|entry| (entry.key.deref(), &entry.value))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&str, &mut V)> {
        self.entries.iter_mut().flatten().map(|entry| (entry.key.deref(), &mut entry.value))
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.iter_mut().map(|(_, v)| v)
    }

    pub fn capacity(&self) -> usize {
        self.entries.len()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len() != 0
    }
}

impl<V> Default for StringTable<V> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
struct Entry<V> {
    key: InternedString,
    value: V,
}

impl<V> Entry<V> {
    fn key(&self) -> &InternedString {
        &self.key
    }

    fn value(&self) -> &V {
        &self.value
    }

    fn value_mut(&mut self) -> &mut V {
        &mut self.value
    }
}

#[cfg(test)]
mod tests {
    use crate::string_interner::StringInterner;

    use super::*;

    #[test]
    fn table() {
        let mut table = StringTable::new();
        let mut interner = StringInterner::with_capacity(8);
        table.insert(interner.intern("hello"), 1);
        table.insert(interner.intern("world"), 2);
        assert_eq!(table.get(interner.intern("world")), Some(&2));
        table.insert(interner.intern("a"), 3);
        table.insert(interner.intern("b"), 4);
        table.insert(interner.intern("c"), 5);
        table.insert(interner.intern("d"), 6);
        table.insert(interner.intern("e"), 7);
        table.insert(interner.intern("f"), 8);

        assert_eq!(table.get(interner.intern("world")), Some(&2));
        assert_eq!(table.get(interner.intern("nope")), None);
        assert_eq!(table.get_mut(interner.intern("world")), Some(&mut 2));
        assert_eq!(table.get_mut(interner.intern("nope")), None);

        table.insert(interner.intern("world"), -123);
        assert_eq!(table.get(interner.intern("world")), Some(&-123));

        *table.get_mut(interner.intern("e")).unwrap() = 123;
        assert_eq!(table.get(interner.intern("e")), Some(&123));
    }

    #[test]
    fn get_str_eq() {
        let s = "s".to_string();
        let mut table = StringTable::new();
        let mut interner = StringInterner::with_capacity(16);

        let s_internered = interner.intern(s.as_str());
        table.insert(s_internered, 123);

        assert_eq!(table.get(s_internered), Some(&123));
        assert_eq!(table.get_str_eq(s_internered), Some(&s_internered));
        assert_eq!(table.get_str_eq(interner.intern("hkjhkjhkj")), None);

        let mut interner2 = StringInterner::with_capacity(16);

        // Clone the string to give it a different address, and then intern it in a second interner,
        // such that the resulting interned string has a different address as well.
        let s_cloned_interned = interner2.intern(&s.clone());
        assert_ne!(s_internered, s_cloned_interned);

        // Now, normal get() returns None because it only compares the address, but get_str_eq()
        // findfs the original interned string since it should do full string comparison.
        assert_eq!(table.get(s_cloned_interned), None);
        assert_eq!(table.get_str_eq(s_cloned_interned), Some(&s_internered));
    }
}

use std::fmt::Display;

use crate::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    items: Vec<Value>,
}

impl List {
    pub fn items(&self) -> &[Value] {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut [Value] {
        &mut self.items
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl FromIterator<Value> for List {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        List { items: iter.into_iter().collect() }
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, v) in self.items.iter().enumerate() {
            write!(f, "{}{v}", if i != 0 { ", " } else { "" })?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

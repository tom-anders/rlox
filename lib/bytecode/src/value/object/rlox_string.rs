use std::rc::Rc;

#[derive(Debug, Clone, Default, Eq)]
pub struct RloxString(pub Rc<String>);

impl<S> From<S> for RloxString where S: Into<Rc<String>> {
    fn from(s: S) -> Self {
        Self(s.into())
    }
}

impl std::hash::Hash for RloxString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
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


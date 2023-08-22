#[derive(Debug)]
pub struct Scope {
    depth: usize,
}

impl Scope {
    pub fn global() -> Self {
        Scope { depth: 0 }
    }

    pub fn next_depth(&self) -> Self {
        Scope { depth: self.depth + 1 }
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn inside_global(&self) -> bool {
        self.depth == 0
    }

    pub fn inside_local(&self) -> bool {
        self.depth > 0
    }

    pub fn begin_scope(&mut self) {
        self.depth += 1;
    }

    pub fn end_scope(&mut self) {
        self.depth -= 1;
    }
}

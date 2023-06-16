use std::fmt::{Display, Debug};

use crate::chunk::{Chunk, StringInterner};

use super::RloxString;

#[derive(Clone, Default)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: RloxString,
}

impl Function {
    pub fn intern_strings<Interner: StringInterner>(&mut self, interner: &mut Interner) {
        interner.intern_string(&mut self.name);
        self.chunk.intern_strings(interner);
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.name.0.as_str() {
            "" => write!(f, "<script>"),
            name => write!(f, "<fn {}>", name),
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let alternate = f.alternate();
        f.debug_struct("Function")
            .field("arity", &self.arity)
            // Printing out the chunk makes output to verbose,
            // do don't to it by default
            .field("chunk", if alternate {
                &self.chunk
            } else {
                &"<chunk>"
            })
            .field("name", &self.name)
            .finish()
    }
}

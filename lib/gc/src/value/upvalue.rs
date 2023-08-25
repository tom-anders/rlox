use std::fmt::{Display, Formatter};

use crate::Value;

#[derive(Clone, Debug, PartialEq)]
pub enum Upvalue {
    // TODO benchmark if using a raw pointer here would be any more performant
    Local { stack_slot: u8 },
    Closed(Value),
}

impl Upvalue {
    pub fn stack_slot(&self) -> Option<u8> {
        match self {
            Upvalue::Local { stack_slot } => Some(*stack_slot),
            Upvalue::Closed(_) => None,
        }
    }
}

impl Display for Upvalue {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        unreachable!("Should not be able to print an upvalue")
    }
}

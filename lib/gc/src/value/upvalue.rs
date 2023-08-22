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

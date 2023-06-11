use std::{mem::size_of, println};

// TODO write a proc_macro to generate this enum
#[repr(C, u8)]
#[derive(Debug, Clone, strum::EnumCount)]
pub enum OpCode {
    Return,
    Constant(Constant),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl From<Constant> for OpCode {
    fn from(op: Constant) -> Self {
        OpCode::Constant(op)
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Return;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Constant {
    pub index: u8,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Negate;

impl OpCode {
    pub fn len(&self) -> usize {
        // +1 for the discriminant/opcode
        1 + match self {
            OpCode::Return => 1,
            OpCode::Constant { .. } => size_of::<Constant>(),
            OpCode::Negate { .. } => 1,
            OpCode::Add  => 1,
            OpCode::Subtract  => 1,
            OpCode::Multiply  => 1,
            OpCode::Divide  => 1,
        }
    }

    pub fn bytes(&self) -> &[u8] {
        // SAFETY: The enum is repr(C, u8), so it's safe to transmute it to a slice of bytes.
        // While the size of the enum is always the size of the largest variant,
        // for smaller variants, we can simply ignore the unused bytes.
        unsafe { std::slice::from_raw_parts(self as *const _ as *const u8, self.len()) }
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        let opcode = bytes[0];
        match opcode {
            0 => OpCode::Return,
            1 => OpCode::Constant(Constant { index: bytes[1] }),
            2 => OpCode::Negate,
            3 => OpCode::Add,
            4 => OpCode::Subtract,
            5 => OpCode::Multiply,
            6 => OpCode::Divide,
            _ => unreachable!("Invalid opcode: {}", opcode),
        }
    }
}


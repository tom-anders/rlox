#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Jump(pub u16);

impl Jump {
    pub fn from_ne_bytes(bytes: [u8; 2]) -> Self {
        Jump(u16::from_ne_bytes(bytes))
    }

    pub fn to_ne_bytes(self) -> [u8; 2] {
        self.0.to_ne_bytes()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, derive_more::Display, derive_more::From)]
pub struct Arity(pub u8);

impl Arity {
    pub fn from_ne_bytes(bytes: [u8; 1]) -> Self {
        Arity(bytes[0])
    }

    pub fn to_ne_bytes(self) -> [u8; 1] {
        [self.0]
    }
}


#[derive(Debug, Clone, PartialEq, instructions_derive::Instruction)]
pub enum Instruction {
    Return,
    Negate,
    Not,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    PopN(u8),
    Constant { index: u8 },
    DefineGlobal { constant_index: u8 },
    SetGlobal { constant_index: u8 },
    GetGlobal { constant_index: u8 },
    SetLocal { stack_slot: u8 },
    GetLocal { stack_slot: u8 },
    JumpIfFalse(Jump),
    Jump(Jump),
    Loop(Jump),
    Call { arg_count: Arity },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_and_from_bytes() {
        let instructions = vec![
            Instruction::Return,
            Instruction::GetLocal { stack_slot: 0x33 },
            Instruction::Jump(Jump(0x1122)),
        ];

        let mut bytes = Vec::new();
        for instruction in &instructions {
            instruction.write_to(&mut bytes);
        }

        bytes.extend_from_slice(&123_u8.to_ne_bytes());

        let decoded = vec![
            Instruction::from_bytes(&bytes[0..1]),
            Instruction::from_bytes(&bytes[1..3]),
            Instruction::from_bytes(&bytes[3..6]),
        ];

        assert_eq!(instructions, decoded);
    }
}

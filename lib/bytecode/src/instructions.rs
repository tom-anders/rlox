#[derive(Debug, Clone, PartialEq, bytecode_derive::Instruction)]
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
}

impl Instruction {
    pub fn from_bytes(bytes: &[u8]) -> Self {
        let opcode: OpCode = bytes[0].into();
        match opcode {
            OpCode::Return => Instruction::Return,
            OpCode::Negate => Instruction::Negate,
            OpCode::Not => Instruction::Not,
            OpCode::Add => Instruction::Add,
            OpCode::Subtract => Instruction::Subtract,
            OpCode::Multiply => Instruction::Multiply,
            OpCode::Divide => Instruction::Divide,
            OpCode::Nil => Instruction::Nil,
            OpCode::True => Instruction::True,
            OpCode::False => Instruction::False,
            OpCode::Equal => Instruction::Equal,
            OpCode::Greater => Instruction::Greater,
            OpCode::Less => Instruction::Less,
            OpCode::Print => Instruction::Print,
            OpCode::Pop => Instruction::Pop,
            OpCode::PopN => Instruction::PopN(bytes[1]),
            OpCode::Constant => Instruction::Constant { index: bytes[1] },
            OpCode::DefineGlobal => Instruction::DefineGlobal { constant_index: bytes[1] },
            OpCode::SetGlobal => Instruction::SetGlobal { constant_index: bytes[1] },
            OpCode::GetGlobal => Instruction::GetGlobal { constant_index: bytes[1] },
            OpCode::SetLocal => Instruction::SetLocal { stack_slot: bytes[1] },
            OpCode::GetLocal => Instruction::GetLocal { stack_slot: bytes[1] },
        }
    }
}

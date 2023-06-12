#[repr(C, u8)]
#[derive(Debug, Clone, PartialEq, bytecode_derive::Instruction)]
pub enum Instruction {
    Return,
    Constant{index: u8},
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Instruction {
    pub fn bytes(&self) -> &[u8] {
        // SAFETY: The enum is repr(C, u8), so it's safe to transmute it to a slice of bytes.
        // While the size of the enum is always the size of the largest variant,
        // for smaller variants, we can simply ignore the unused bytes.
        unsafe { std::slice::from_raw_parts(self as *const _ as *const u8, self.num_bytes()) }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_to_and_from_bytes() {
        use super::Instruction;

        let instructions = vec![
            Instruction::Return,
            Instruction::Constant{index: 123},
            Instruction::Negate,
            Instruction::Add,
            Instruction::Subtract,
            Instruction::Multiply,
            Instruction::Divide,
        ];

        for instruction in instructions {
            let bytes = instruction.bytes();
            let new_instruction = Instruction::from_bytes(bytes);
            assert_eq!(instruction, new_instruction);
        }
    }
}

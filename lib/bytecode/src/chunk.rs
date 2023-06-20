use std::{fmt::Debug, mem::size_of, rc::Rc, writeln};

use crate::{
    instructions::*,
    string_interner::StringInterner,
    value::{RloxString, Value},
};

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn write_instructions(
        &mut self,
        instructions: impl IntoIterator<Item = Instruction>,
        line: cursor::Line,
    ) -> usize {
        for instruction in instructions {
            self.write_instruction(instruction, line);
        }
        self.code().len() - 1
    }

    pub fn write_instruction(&mut self, instr: Instruction, line: cursor::Line) -> usize {
        instr.write_to(&mut self.code);

        // TODO: This is a waste of memory of course, challenge 1 in chapter 14 would solve this.
        for _ in 0..instr.num_bytes() {
            self.lines.push(line.0);
        }

        self.code().len() - 1
    }

    pub fn patch_jump(&mut self, offset: usize, jump: Jump) {
        self.code[offset..offset + size_of::<Jump>()].copy_from_slice(&jump.0.to_ne_bytes());
    }

    pub fn add_constant(&mut self, value: Value) -> Option<u8> {
        self.constants.push(value);
        (self.constants.len() - 1).try_into().ok()
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn lines(&self) -> &[usize] {
        &self.lines
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn get_string_constant(&self, index: u8) -> Option<&RloxString> {
        self.constants.get(index as usize).and_then(|v| v.try_into().ok())
    }

    pub fn disassemble_instruction(
        &self,
        offset: usize,
        interner: &StringInterner,
    ) -> (String, usize) {
        let instr = Instruction::from_bytes(&self.code[offset..]);

        let line = self.lines[offset];
        let line_str = if offset > 0 && line == self.lines[offset - 1] {
            "   |".to_string()
        } else {
            format!("{:4}", line)
        };

        let get_constant = |index: u8| {
            format!(
                "{index} -> {}",
                self.constants
                    .get(index as usize)
                    .map(|c| c.with_interner(interner).to_string())
                    .unwrap_or_else(|| "??".to_string())
            )
        };

        let op_args = match instr {
            Instruction::PopN(n) => format!("'{n}'"),
            Instruction::Constant { index } => get_constant(index),
            Instruction::DefineGlobal { constant_index } => get_constant(constant_index),
            Instruction::SetGlobal { constant_index } => get_constant(constant_index),
            Instruction::GetGlobal { constant_index } => get_constant(constant_index),
            Instruction::SetLocal { stack_slot } => format!("'{stack_slot}'"),
            Instruction::GetLocal { stack_slot } => format!("'{stack_slot}'"),
            Instruction::JumpIfFalse(jump) => format!("'{}'", jump.0),
            Instruction::Jump(jump) => format!("'{}'", jump.0),
            Instruction::Loop(jump) => format!("'{}'", jump.0),
            Instruction::Call { arg_count } => format!("'{}'", arg_count),
            Instruction::Return
            | Instruction::Negate
            | Instruction::Not
            | Instruction::Add
            | Instruction::Subtract
            | Instruction::Multiply
            | Instruction::Divide
            | Instruction::Nil
            | Instruction::True
            | Instruction::False
            | Instruction::Equal
            | Instruction::Greater
            | Instruction::Less
            | Instruction::Print
            | Instruction::Pop => "".to_string(),
        };

        let opcode: OpCode = instr.opcode();

        (format!("{offset:04} {line_str} {opcode:16} {op_args}"), offset + instr.num_bytes())
    }

    pub fn with_interner<'a, 'b>(&'a self, interner: &'b StringInterner) -> ChunkWithInterner<'a, 'b> {
        ChunkWithInterner(self, interner)
    }
}

pub struct ChunkWithInterner<'a, 'b>(&'a Chunk, &'b StringInterner);

impl Debug for ChunkWithInterner<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut offset = 0;

        writeln!(f)?;
        writeln!(f, "================ Chunk ================")?;
        while offset < self.0.code.len() {
            let (debug, new_offset) = self.0.disassemble_instruction(offset, self.1);
            writeln!(f, "{}", debug)?;
            offset = new_offset;
        }
        writeln!(f, "=======================================")?;

        Ok(())
    }
}

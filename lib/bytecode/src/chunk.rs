use std::{collections::HashSet, fmt::Debug, println, rc::Rc, writeln, mem::size_of};

use cursor::Line;
use itertools::Itertools;

use crate::{
    instructions::*,
    value::{ObjectData, Value},
};

#[derive(Clone, Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

pub trait StringInterner {
    fn intern_string(&mut self, string: &mut Rc<String>);
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
        let opcode = instr.opcode();
        self.code.push(opcode as u8);

        use Instruction::*;
        match instr {
            Constant { index } => self.code.push(index),
            DefineGlobal { constant_index } => self.code.push(constant_index),
            SetGlobal { constant_index } => self.code.push(constant_index),
            GetGlobal { constant_index } => self.code.push(constant_index),
            SetLocal { stack_slot } => self.code.push(stack_slot),
            GetLocal { stack_slot } => self.code.push(stack_slot),
            JumpIfFalse(jump) => {
                self.code.extend_from_slice(&jump.0.to_ne_bytes());
            }
            PopN(n) => self.code.push(n),
            Return | Negate | Not | Add | Subtract | Multiply | Divide | Nil | True | False
            | Equal | Greater | Less | Print | Pop => {}
        };

        // TODO: This is a waste of memory of course, challenge 1 in chapter 14 would solve this.
        for _ in 0..instr.num_bytes() {
            self.lines.push(line.0);
        }

        self.code().len() - 1
    }

    pub fn patch_jump(&mut self, offset: usize, jump: Jump) {
        self.code[offset..offset + size_of::<Jump>()].copy_from_slice(&jump.0.to_ne_bytes());
    }

    pub fn intern_strings<Interner: StringInterner>(&mut self, interner: &mut Interner) {
        for constant in self.constants.iter_mut() {
            constant.intern_string(interner);
        }
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

    pub fn get_string_constant(&self, index: u8) -> Option<&Rc<String>> {
        self.constants.get(index as usize).and_then(|v| match v {
            Value::Object(o) => match o.data() {
                ObjectData::String(s) => Some(s),
                _ => None,
            },
            _ => None,
        })
    }

    pub fn disassemble_instruction(&self, offset: usize) -> (String, usize) {
        let instr = Instruction::from_bytes(&self.code[offset..]);

        let line = self.lines[offset];
        let line_str = if offset > 0 && line == self.lines[offset - 1] {
            "   |".to_string()
        } else {
            format!("{:4}", line)
        };

        let op_args = match instr {
            Instruction::PopN(n) => format!("'{n}'"),
            Instruction::Constant { index } => format!(
                "{index} '{}'",
                self.constants
                    .get(index as usize)
                    .map(|c| c.to_string())
                    .unwrap_or_else(|| "??".to_string())
            ),
            Instruction::DefineGlobal { constant_index } => format!("'{constant_index}'"),
            Instruction::SetGlobal { constant_index } => format!("'{constant_index}'"),
            Instruction::GetGlobal { constant_index } => format!("'{constant_index}'"),
            Instruction::SetLocal { stack_slot } => format!("'{stack_slot}'"),
            Instruction::GetLocal { stack_slot } => format!("'{stack_slot}'"),
            Instruction::JumpIfFalse(jump) => format!("'{}'", jump.0),
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
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut offset = 0;

        writeln!(f)?;
        writeln!(f, "================ Chunk ================")?;
        while offset < self.code.len() {
            let (debug, new_offset) = self.disassemble_instruction(offset);
            writeln!(f, "{}", debug)?;
            offset = new_offset;
        }
        writeln!(f, "=======================================")?;

        Ok(())
    }
}

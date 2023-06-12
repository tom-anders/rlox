use std::{fmt::Debug, println, writeln};

use cursor::Line;
use itertools::Itertools;

use crate::{instructions::*, value::Value};

#[derive(Clone, Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn write_instructions(&mut self, instructions: impl IntoIterator<Item = (Instruction, Line)>) {
        for (instruction, line) in instructions {
            self.write_instruction(instruction, line);
        }
    }

    pub fn write_instruction(&mut self, op: Instruction, line: cursor::Line) {
        self.code.extend_from_slice(op.bytes());

        // TODO: This is a waste of memory of course, challenge 1 in chapter 14 would solve this.
        for _ in 0..op.num_bytes() {
            self.lines.push(line.0);
        }
    }

    pub fn add_constant(&mut self, value: Value) -> Option<Instruction> {
        self.constants.push(value);
        let index = (self.constants.len() - 1).try_into().ok()?;
        Some(Instruction::Constant{ index })
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn disassemble_instruction(&self, offset: usize) -> (String, usize) {
        let op = Instruction::from_bytes(&self.code[offset..]);

        let line = self.lines[offset];
        let line_str = if offset > 0 && line == self.lines[offset - 1] {
            "   |".to_string()
        } else {
            format!("{:4}", line)
        };

        let (op_str, op_args) = match op {
            Instruction::Return => ("Return", "".to_string()),
            Instruction::Constant{ index } => (
                "Constant",
                format!(
                    "{index} '{}'",
                    self.constants
                        .get(index as usize)
                        .map(|c| c.to_string())
                        .unwrap_or_else(|| "??".to_string())
                ),
            ),
            Instruction::Negate => ("Negate", "".to_string()),
            Instruction::Add => ("Add", "".to_string()),
            Instruction::Subtract => ("Subtract", "".to_string()),
            Instruction::Multiply => ("Multiply", "".to_string()),
            Instruction::Divide => ("Divide", "".to_string()),
        };

        (format!("{offset:04} {line_str} {op_str:16} {op_args}"), offset + op.num_bytes())
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

#[cfg(test)]
mod tests {
    use cursor::Line;

    use super::*;

    #[test]
    fn debug() {
        let mut chunk = Chunk::default();

        let c = chunk.add_constant(Value::Number(1.2)).unwrap();
        chunk.write_instruction( c, Line(1));
        let c = chunk.add_constant(Value::Number(3.4)).unwrap();
        chunk.write_instruction(c, Line(1));

        chunk.write_instruction(Instruction::Return, Line(1));

        dbg!(chunk);
    }
}

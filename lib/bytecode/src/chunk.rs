use std::{fmt::Debug, println, writeln};

use itertools::Itertools;

use crate::{opcode::*, value::Value};

#[derive(Clone, Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn write_instruction(&mut self, op: OpCode, line: usize) {
        self.code.extend_from_slice(op.bytes());

        // TODO: This is a waste of memory of course, challenge 1 in chapter 14 would solve this.
        for _ in 0..op.len() {
            self.lines.push(line);
        }
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1).try_into().expect("Too many constants!")
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn disassemble_instruction(&self, offset: usize) -> (String, usize) {
        let op = OpCode::from_bytes(&self.code[offset..]);

        let line = self.lines[offset];
        let line_str = if offset > 0 && line == self.lines[offset - 1] {
            "   |".to_string()
        } else {
            format!("{:4}", line)
        };

        let (op_str, op_args) = match op {
            OpCode::Return => ("Return", "".to_string()),
            OpCode::Constant(Constant { index }) => (
                "Constant",
                format!(
                    "{index} '{}'",
                    self.constants
                        .get(index as usize)
                        .map(|c| c.to_string())
                        .unwrap_or_else(|| "??".to_string())
                ),
            ),
            OpCode::Negate => ("Negate", "".to_string()),
            OpCode::Add => ("Add", "".to_string()),
            OpCode::Subtract => ("Subtract", "".to_string()),
            OpCode::Multiply => ("Multiply", "".to_string()),
            OpCode::Divide => ("Divide", "".to_string()),
        };

        (format!("{offset:04} {line_str} {op_str:16} {op_args}"), offset + op.len())
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
    use super::*;

    #[test]
    fn debug() {
        let mut chunk = Chunk::default();

        let c = Constant{index: chunk.add_constant(Value::Number(1.2))}.into();
        chunk.write_instruction( c, 1);
        let c = Constant{index: chunk.add_constant(Value::Number(3.4))}.into();
        chunk.write_instruction(c, 1);
        chunk.write_instruction(OpCode::Return, 1);

        dbg!(chunk);
    }
}

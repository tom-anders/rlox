use std::{fmt::Debug, mem::size_of, ops::Deref, writeln};

use instructions::{Instruction, Jump, OpCode};
use itertools::Itertools;
use strings::string_interner::InternedString;

use crate::{FunctionRef, Value};

#[derive(Clone, Default, PartialEq)]
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

    pub fn write_bytes(&mut self, bytes: impl Iterator<Item = u8>, line: cursor::Line) -> usize {
        for byte in bytes {
            self.code.push(byte);
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

    pub fn get_string_constant(&self, index: u8) -> InternedString {
        *self
            .constants()
            .get(index as usize)
            .expect("Missing string constant")
            .clone()
            .unwrap_object()
            .unwrap_string()
            .deref()
    }

    pub fn get_function_constant(&self, index: u8) -> FunctionRef {
        self.constants()
            .get(index as usize)
            .expect("Missing function constant")
            .clone()
            .unwrap_object()
            .unwrap_function()
    }

    pub fn disassemble_instruction(&self, offset: usize) -> (String, usize) {
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
                    .map(Value::to_string)
                    .unwrap_or_else(|| "??".to_string())
            )
        };

        let op_args = match instr {
            Instruction::PopN(n) => format!("'{n}'"),
            Instruction::Constant { index } => get_constant(index),
            Instruction::Closure { constant_index, upvalue_count } => {
                let upvalues = &self.code[offset + 3..offset + 3 + 2 * upvalue_count as usize]
                    .chunks(2)
                    .enumerate()
                    .map(|(i, chunk)| {
                        let (is_local, index) = (chunk[0], chunk[1]);
                        format!(
                            "{:04}      |                     {} {}",
                            offset + 3 + 2 * i,
                            if is_local == 0 { "local" } else { "upvalue" },
                            index
                        )
                    })
                    .collect_vec()
                    .join("\n");
                format!("{}\n{}", get_constant(constant_index), upvalues)
            }
            Instruction::DefineGlobal { constant_index } => get_constant(constant_index),
            Instruction::SetGlobal { constant_index } => get_constant(constant_index),
            Instruction::GetGlobal { constant_index } => get_constant(constant_index),
            Instruction::SetProperty { constant_index } => get_constant(constant_index),
            Instruction::GetProperty { constant_index } => get_constant(constant_index),
            Instruction::Class { constant_index } => get_constant(constant_index),
            Instruction::Method { constant_index } => get_constant(constant_index),
            Instruction::GetSuper { constant_index: name } => get_constant(name),
            Instruction::SetLocal { stack_slot } => format!("'{stack_slot}'"),
            Instruction::GetLocal { stack_slot } => format!("'{stack_slot}'"),
            Instruction::GetUpvalue { upvalue_index } => format!("'{}'", upvalue_index),
            Instruction::SetUpvalue { upvalue_index } => format!("'{}'", upvalue_index),
            Instruction::JumpIfFalse(jump) => format!("'{}'", jump.0),
            Instruction::Jump(jump) => format!("'{}'", jump.0),
            Instruction::Loop(jump) => format!("'{}'", jump.0),
            Instruction::Call { arg_count } => format!("'{}'", arg_count),
            Instruction::Invoke { constant_index, arg_count } => {
                format!("{} '{}'", get_constant(constant_index), arg_count)
            }
            Instruction::InvokeSuper { constant_index, arg_count } => {
                format!("{} '{}'", get_constant(constant_index), arg_count)
            }
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
            | Instruction::CloseUpvalue
            | Instruction::Inherit
            | Instruction::Pop => "".to_string(),
        };

        let opcode: OpCode = instr.opcode();

        (format!("{offset:04} {line_str} {opcode:16} {op_args}"), offset + instr.num_bytes())
    }

    pub fn constants_mut(&mut self) -> &mut Vec<Value> {
        &mut self.constants
    }

    pub fn mark_reachable(&mut self) {
        for constant in &mut self.constants {
            constant.mark_reachable();
        }
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

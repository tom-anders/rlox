use std::fmt::Display;

use gc::{Chunk, Value, StringInterner, ClosureRef};
use instructions::Arity;
use itertools::Itertools;
use log::trace;

const MAX_FRAMES: usize = 64;
const MAX_STACK: usize = MAX_FRAMES * u8::MAX as usize;

#[derive(Debug, Clone)]
pub struct CallFrame {
    closure: ClosureRef,
    // FIXME: Using a raw pointer would be a bit more performant, but would also require `unsafe`.
    // So let's leave it like this for now and maybe optimize later.
    ip: usize,
    base_slot: usize,
}

impl CallFrame {
    pub fn new(closure: ClosureRef, base_slot: usize) -> Self {
        Self { closure, ip: 0, base_slot }
    }

    pub fn closure(&self) -> &ClosureRef {
        &self.closure
    }

    pub fn closure_mut(&mut self) -> &mut ClosureRef {
        &mut self.closure
    }

    pub fn decr_ip(&mut self, offset: usize) {
        self.ip -= offset;
    }

    pub fn inc_ip(&mut self, offset: usize) {
        self.ip += offset;
    }

    pub fn ip(&self) -> usize {
        self.ip
    }

    pub fn base_slot(&self) -> usize {
        self.base_slot
    }
}

#[derive(Debug)]
pub struct Stack {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
}

pub struct StackResolved<'a, 'b>(&'a Stack, &'b StringInterner);

impl Display for StackResolved<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Stack {{stack: [{}], call_frames: [{}] }}",
            self.0.stack
                .iter()
                .map(|v| v.resolve(self.1).to_string())
                .collect_vec()
                .join(", "),
            self.0.frames().iter().map(|frame| format!("{:?}", frame)).collect_vec().join(", ")
        )
    }
}

impl Stack {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(MAX_STACK),
            frames: Vec::with_capacity(MAX_FRAMES),
        }
    }

    pub fn resolve<'a, 'b>(&'a self, interner: &'b StringInterner) -> StackResolved<'a, 'b> {
        StackResolved(self, interner)
    }

    pub fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    pub fn frame_chunk(&self) -> &Chunk {
        self.frame().closure.chunk()
    }

    pub fn current_line(&self) -> usize {
        self.frame_chunk().lines()[self.frame().ip()]
    }

    pub fn frames(&self) -> &[CallFrame] {
        &self.frames
    }

    pub fn frames_mut(&mut self) -> &mut [CallFrame] {
        &mut self.frames
    }

    pub fn push_frame(&mut self, closure: ClosureRef) {
        assert!(self.frames.len() < MAX_FRAMES, "Stack overflow");
        self.frames.push(CallFrame::new(closure.clone(), self.stack.len() - closure.arity().0 as usize - 1));
    }

    pub fn pop_frame(&mut self) -> CallFrame {
        let popped_frame = self.frames.pop().expect("Stack underflow");
        // +1 for the function itself
        self.pop_n(popped_frame.closure.arity().0 as usize + 1);

        popped_frame
    }

    pub fn truncate_stack(&mut self, len: usize) {
        self.stack.truncate(len);
    }

    pub fn push(&mut self, value: Value) {
        assert!(self.stack.len() < MAX_STACK, "Stack overflow");
        trace!("Pushing {:?}", value);
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Value {
        let value = self.stack.pop().expect("Stack underflow");
        trace!("Popping {:?}", value);
        value
    }

    pub fn global_stack_at_mut(&mut self, index: u8) -> &mut Value {
        self.stack
            .get_mut(index as usize)
            .unwrap_or_else(|| panic!("Invalid stack index: {}", index))
    }
    pub fn global_stack_at(&self, index: u8) -> &Value {
        self.stack
            .get(index as usize)
            .unwrap_or_else(|| panic!("Invalid stack index: {}", index))
    }

    pub fn stack_at(&self, index: u8) -> &Value {
        let index = self.frame().base_slot + index as usize;
        self.stack
            .get(index)
            .unwrap_or_else(|| panic!("Invalid stack index: {}", index))
    }

    pub fn stack_at_mut(&mut self, index: u8) -> &mut Value {
        let index = self.frame().base_slot + index as usize;
        self.stack
            .get_mut(index)
            .unwrap_or_else(|| panic!("Invalid stack index: {}", index))
    }

    pub fn pop_n(&mut self, n: usize) {
        debug_assert!(n <= self.stack.len());
        self.stack.truncate(self.stack.len() - n);
    }

    pub fn peek(&self) -> &Value {
        self.stack.last().as_ref().unwrap()
    }

    pub fn peek_mut(&mut self) -> &mut Value {
        self.stack.last_mut().unwrap()
    }

    pub fn len(&self) -> u8 {
        self.stack.len() as u8
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Value> {
        self.stack.iter_mut()
    }

    pub fn peek_n(&self, n: usize) -> impl Iterator<Item = &Value> {
        self.stack[self.stack.len() - 1 - n..].iter()
    }

    pub fn stack_trace(&self, interner: &StringInterner) -> String {
        self.frames()
            .iter()
            .rev()
            .map(|frame| {
                let function = &*frame.closure.function();
                let name = if function.name.resolve(interner).is_empty() {
                    "script".to_string()
                } else {
                    format!("{}()", function.name.resolve(interner))
                };
                format!("[line {}] in {}", function.chunk.lines()[frame.ip()], name)
            })
            .collect_vec()
            .join("\n")
    }
}

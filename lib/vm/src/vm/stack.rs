use std::fmt::Display;

use gc::{Chunk, Value, StringInterner, ClosureRef};
use instructions::Arity;
use itertools::Itertools;
use log::trace;

mod array_stack;
pub use array_stack::ArrayStack;

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

pub type ValueStack = ArrayStack<Value, { u8::MAX as usize }>;
pub type CallStack = ArrayStack<CallFrame, 64>;

#[derive(Debug)]
pub struct Stack {
    values: ValueStack,
    frames: CallStack,
}

pub struct StackResolved<'a, 'b>(&'a Stack, &'b StringInterner);

impl Display for StackResolved<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Stack {{stack: [{}], call_frames: [{}] }}",
            self.0.values
                .iter()
                .map(|v| v.resolve(self.1).to_string())
                .collect_vec()
                .join(", "),
            self.0.iter_frames().map(|frame| format!("{:?}", frame)).collect_vec().join(", ")
        )
    }
}

impl Stack {
    pub fn new() -> Self {
        Self {
            values: ArrayStack::new(),
            frames: ArrayStack::new(),
        }
    }

    pub fn resolve<'a, 'b>(&'a self, interner: &'b StringInterner) -> StackResolved<'a, 'b> {
        StackResolved(self, interner)
    }

    pub fn frame(&self) -> &CallFrame {
        self.frames.peek()
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.peek_mut()
    }

    pub fn frame_chunk(&self) -> &Chunk {
        self.frame().closure.chunk()
    }

    pub fn current_line(&self) -> usize {
        self.frame_chunk().lines()[self.frame().ip()]
    }

    pub fn iter_frames(&self) -> impl DoubleEndedIterator<Item = &CallFrame> {
        self.frames.iter()
    }

    pub fn frames(&self) -> &CallStack {
        &self.frames
    }

    pub fn iter_frames_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut CallFrame> {
        self.frames.iter_mut()
    }

    pub fn push_frame(&mut self, closure: ClosureRef) {
        self.frames.push(CallFrame::new(closure.clone(), self.values.len() - closure.arity().0 as usize - 1));
    }

    pub fn pop_frame(&mut self) -> CallFrame {
        let popped_frame = self.frames.pop();
        // +1 for the function itself
        self.pop_n(popped_frame.closure.arity().0 as usize + 1);

        popped_frame
    }

    pub fn truncate_stack(&mut self, len: usize) {
        self.values.truncate(len);
    }

    pub fn push(&mut self, value: Value) {
        trace!("Pushing {:?}", value);
        self.values.push(value);
    }

    pub fn pop(&mut self) -> Value {
        let value = self.values.pop();
        trace!("Popping {:?}", value);
        value
    }

    pub fn global_stack_at_mut(&mut self, index: u8) -> &mut Value {
        &mut self.values[index as usize]
    }
    pub fn global_stack_at(&self, index: u8) -> &Value {
        &self.values[index as usize]
    }

    pub fn frame_stack_at(&self, index: u8) -> &Value {
        let index = self.frame().base_slot + index as usize;
        &self.values[index]
    }

    pub fn frame_stack_at_mut(&mut self, index: u8) -> &mut Value {
        let index = self.frame().base_slot + index as usize;
        &mut self.values[index]
    }

    pub fn pop_n(&mut self, n: usize) {
        self.values.pop_n(n)
    }

    pub fn peek(&self) -> &Value {
        self.values.peek()
    }

    pub fn len(&self) -> u8 {
        self.values.len() as u8
    }

    pub fn iter(&mut self) -> impl DoubleEndedIterator<Item = &Value> {
        self.values.iter()
    }

    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Value> {
        self.values.iter_mut()
    }

    pub fn value_stack(&self) -> &ValueStack {
        &self.values
    }

    pub fn stack_trace(&self, interner: &StringInterner) -> String {
        self.iter_frames()
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

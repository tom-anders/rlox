use std::{
    debug_assert,
    mem::MaybeUninit,
    ops::{Index, IndexMut},
};

use itertools::Itertools;

pub struct ArrayStack<T, const N: usize> {
    stack: [MaybeUninit<T>; N],
    top: usize,
}

impl<T: std::fmt::Debug, const N: usize> std::fmt::Debug for ArrayStack<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.iter().collect_vec().as_slice())
    }
}

impl<T, const N: usize> ArrayStack<T, N> {
    pub fn new() -> Self {
        Self {
            // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#initializing-an-array-element-by-element
            stack: unsafe { MaybeUninit::uninit().assume_init() },
            top: 0,
        }
    }

    #[must_use]
    pub fn push(&mut self, t: T) -> Option<()> {
        if self.top == N {
            None // stack overflow
        } else {
            self.stack[self.top].write(t);

            self.top += 1;

            Some(())
        }
    }

    pub fn pop(&mut self) -> T {
        debug_assert!(self.top != 0, "stack underflow");
        self.top -= 1;

        unsafe { self.stack[self.top].assume_init_read() }
    }

    pub fn pop_n(&mut self, n: usize) -> impl Iterator<Item = T> + '_ {
        debug_assert!(n <= self.len());
        self.top -= n;
        unsafe {
            // SAFETY: Before pop_n, we had n initialized elements at the top of the stack,
            // so it is safe to read them (even though we lowered the top pointer).
            (0..n).map(move |i| self.stack[self.top + i].assume_init_read() )
        }
    }

    pub fn clear(&mut self) {
        self.top = 0;
    }

    pub fn len(&self) -> usize {
        self.top
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn peek(&self) -> &T {
        unsafe { self.stack[self.top - 1].assume_init_ref() }
    }

    pub fn peek_mut(&mut self) -> &mut T {
        unsafe { self.stack[self.top - 1].assume_init_mut() }
    }

    pub fn truncate(&mut self, new_size: usize) {
        self.top = new_size;
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.stack[..self.top].iter().map(|v| unsafe { v.assume_init_ref() })
    }

    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut T> {
        self.stack[..self.top].iter_mut().map(|v| unsafe { v.assume_init_mut() })
    }

    pub fn peek_nth(&self, n: usize) -> &T {
        unsafe { self.stack[self.top - 1 - n].assume_init_ref() }
    }
}

impl<T, const N: usize> Index<usize> for ArrayStack<T, N> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len(), "invalid stack index: {index}");
        unsafe { self.stack[index].assume_init_ref() }
    }
}

impl<T, const N: usize> IndexMut<usize> for ArrayStack<T, N> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.len());
        unsafe { self.stack[index].assume_init_mut() }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use itertools::Itertools;

    use super::*;

    fn make_stack() -> ArrayStack<usize, 8> {
        ArrayStack::new()
    }
    #[test]
    #[should_panic]
    fn stack_overflow() {
        let mut stack = ArrayStack::<usize, 2>::new();
        stack.push(1).unwrap();
        stack.push(1).unwrap();
        stack.push(1).unwrap();
    }

    #[test]
    #[should_panic]
    fn stack_underflow() {
        let mut stack = make_stack();
        stack.push(1).unwrap();
        stack.pop();
        stack.pop();
    }

    #[test]
    fn push_and_pop() {
        let mut stack = ArrayStack::<usize, 8>::new();
        assert_eq!(stack.len(), 0);
        assert!(stack.is_empty());

        stack.push(1).unwrap();
        stack.push(2).unwrap();
        stack.push(3).unwrap();
        assert_eq!(stack.len(), 3);
        assert!(!stack.is_empty());

        assert_eq!(stack.peek(), &3);

        assert_eq!(stack.iter().collect_vec(), vec![&1, &2, &3]);

        for v in stack.iter_mut() {
            *v += 1;
        }

        assert_eq!(stack.iter().collect_vec(), vec![&2, &3, &4]);

        assert_eq!(stack.peek(), &4);
        assert_eq!(stack.peek_nth(1), &3);

        assert_eq!(stack.pop_n(2).collect_vec(), vec![3, 4]);
        assert_eq!(stack.iter().collect_vec(), vec![&2]);
    }
}

use std::{ops::Deref, cell::RefCell, rc::Rc};

use crate::{Closure, Function, NativeFun, RloxString, StringInterner, Upvalue};

#[derive(Debug, PartialEq, derive_more::TryInto, derive_more::From)]
#[try_into(owned, ref, ref_mut)]
pub enum Object {
    String(RloxString),
    Function(Function),
    NativeFun(NativeFun),
    Closure(Closure),
    Upvalue(Upvalue),
}

impl Object {
    pub fn resolve<'a, 'b>(&'a self, interner: &'b StringInterner) -> ObjectWithInterner<'a, 'b> {
        ObjectWithInterner(self, interner)
    }
}

pub struct ObjectWithInterner<'a, 'b>(&'a Object, &'b StringInterner);

impl std::fmt::Debug for ObjectWithInterner<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Object::String(s) => write!(f, "String({:?})", s.resolve(self.1)),
            Object::Function(fun) => write!(f, "Function({:?})", fun.resolve(self.1)),
            Object::Closure(closure) => write!(
                f,
                "Closure(function: {:?}, upvalues: {:?})",
                closure.function().resolve(self.1),
                closure.upvalues().iter().map(|u| u.deref()).collect::<Vec<_>>()
            ),
            Object::NativeFun(fun) => write!(f, "NativeFun<{:?}>", fun),
            Object::Upvalue(upvalue) => write!(f, "{:?}", upvalue),
        }
    }
}

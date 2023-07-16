use std::{cell::RefCell, ops::Deref, rc::Rc};

use itertools::Itertools;

use crate::{Class, Closure, Function, Instance, NativeFun, RloxString, StringInterner, Upvalue, BoundMethod};

#[derive(Debug, PartialEq, derive_more::TryInto, derive_more::From)]
#[try_into(owned, ref, ref_mut)]
pub enum Object {
    String(RloxString),
    Function(Function),
    NativeFun(NativeFun),
    Closure(Closure),
    Upvalue(Upvalue),
    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
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
            Object::Class(class) => write!(f, "Class({:?})", class.name().resolve(self.1)),
            Object::Instance(instance) => write!(
                f,
                "Instance({:?}, fields: {:?})",
                instance.class().name().resolve(self.1),
                instance
                    .fields()
                    .iter()
                    .map(|(name, field)| format!("{}: {:?}", name.resolve(self.1), field.resolve(self.1)))
                    .collect_vec()
            ),
            Object::BoundMethod(bound_method) => write!(
                f,
                "BoundMethod({:?}, receiver: {:?})",
                Object::Closure(bound_method.method().deref().clone()).resolve(self.1),
                Object::Instance(bound_method.receiver().deref().clone()).resolve(self.1),
            ),
        }
    }
}

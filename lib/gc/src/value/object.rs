use strings::string_interner::InternedString;

use crate::{BoundMethod, Class, Closure, Function, Instance, List, NativeFun, Upvalue};

#[derive(Debug, PartialEq, derive_more::TryInto, derive_more::From, derive_more::Display)]
#[try_into(owned, ref, ref_mut)]
pub enum Object {
    String(InternedString),
    List(List),
    Function(Function),
    NativeFun(NativeFun),
    Closure(Closure),
    Upvalue(Upvalue),
    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
}

use crate::{RloxString, Function, NativeFun, Closure};

#[derive(Debug, Clone, PartialEq, derive_more::TryInto, derive_more::From)]
#[try_into(owned, ref, ref_mut)]
pub enum Object {
    String(RloxString),
    Function(Function),
    NativeFun(NativeFun),
    Closure(Closure),
}


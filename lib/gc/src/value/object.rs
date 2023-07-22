use crate::{Class, Closure, Function, Instance, NativeFun, Upvalue, BoundMethod, InternedString};

#[derive(Debug, PartialEq, derive_more::TryInto, derive_more::From)]
#[try_into(owned, ref, ref_mut)]
pub enum Object {
    String(InternedString),
    Function(Function),
    NativeFun(NativeFun),
    Closure(Closure),
    Upvalue(Upvalue),
    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Function(fun) => write!(f, "<fn {}>", fun.name),
            Object::Closure(closure) => {
                write!(f, "<fn {}>", closure.function().name)
            }
            Object::NativeFun(native_fn) => write!(f, "<native fn {:?}>", native_fn),
            Object::Upvalue(_) => unreachable!("Should not be able to print an upvalue"),
            Object::Class(class) => write!(f, "{}", class.name()),
            Object::Instance(instance) => {
                write!(f, "{} instance", instance.class().name())
            }
            Object::BoundMethod(bound_method) => {
                write!(f, "{}", bound_method.method().function().name)
            }
        }
    }
}

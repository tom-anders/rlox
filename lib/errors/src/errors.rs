use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use cursor::{Col, Line};

#[derive(thiserror::Error, Clone, Debug, PartialEq)]
#[error("error (l. {line}, c. {col}): {message}")]
pub struct RloxError {
    pub line: Line,
    pub col: Col,
    pub message: String,
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub struct RloxErrors(pub Vec<RloxError>);

impl<E> From<E> for RloxErrors where E: Into<RloxError> {
    fn from(e: E) -> Self {
        Self(vec![e.into()])
    }
}

impl Deref for RloxErrors {
    type Target = Vec<RloxError>;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute(&self.0) }
    }
}

impl DerefMut for RloxErrors {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for RloxErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n"))
    }
}

pub type Result<T> = std::result::Result<T, RloxError>;

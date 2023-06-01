use std::{write, fmt::Display};

use itertools::Itertools;

#[derive(Debug)]
pub struct Token {}

#[derive(thiserror::Error, Debug)]
enum Error {

}

#[derive(thiserror::Error, Debug)]
pub struct Errors(Vec<Error>);

impl Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|e| e.to_string()).join("\n"))
    }
}

pub fn scan_tokens<'a>(chars: impl Iterator<Item = char> + 'a) -> Result<Vec<Token>, Errors> {
    todo!()
}

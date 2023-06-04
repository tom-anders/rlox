use std::collections::HashMap;

use scanner::Token;

use crate::value::Value;

#[derive(Debug, Default)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get<'a>(&'a self, token: &Token) -> Option<&'a Value> {
        self.values.get(token.lexeme)
    }

    pub fn assign(&mut self, token: &Token, value: Value) -> bool {
        if self.values.contains_key(token.lexeme) {
            self.values.insert(token.lexeme.to_string(), value);
            true
        } else {
            false
        }
    }
}

use std::{collections::HashMap, cell::RefCell, rc::Rc};

use scanner::Token;

use crate::value::Value;

#[derive(Debug)]
pub struct Environment {
    scopes: Vec<HashMap<String, Value>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }
}

impl Environment {
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.scopes.last_mut().unwrap().insert(name.to_string(), value);
    }

    pub fn get<'a>(&'a self, token: &Token) -> Option<&'a Value> {
        self.scopes.iter().rev().find_map(|values| values.get(token.lexeme()))
    }

    pub fn assign(&mut self, token: &Token, value: Value) -> bool {
        match self.scopes.iter_mut().rev().find(|v| v.contains_key(token.lexeme())) {
            Some(scope) => {
                scope.insert(token.lexeme().to_string(), value);
                true
            },
            None => false,
        }
    }
}

pub struct Scope(Rc<RefCell<Environment>>);

impl Scope {
    pub fn new(env: Rc<RefCell<Environment>>) -> Self {
        env.borrow_mut().push_scope();
        Self(env)
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        self.0.borrow_mut().pop_scope();
    }
}

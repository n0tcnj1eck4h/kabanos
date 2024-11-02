use std::collections::{HashMap, VecDeque};
use std::default::Default;

#[derive(Default)]
pub struct SymbolTable<S, F> {
    pub scope_stack: VecDeque<HashMap<String, S>>,
    pub functions: HashMap<String, F>,
}

impl<S, F> SymbolTable<S, F> {
    pub fn push_value(&mut self, name: &str, symbol: S) {
        self.scope_stack
            .back_mut()
            .expect("There is no stack to put local var in")
            .insert(name.into(), symbol);
    }

    pub fn push_scope(&mut self) {
        self.scope_stack.push_back(Default::default());
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop_back();
    }

    pub fn get_value(&self, name: &str) -> Option<S> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(*value);
            }
        }
        None
    }

    pub fn add_function(&mut self, name: String, function: F) {
        self.functions.insert(name, function);
    }

    pub fn get_function(&self, name: &str) -> Option<&F> {
        self.functions.get(name)
    }
}

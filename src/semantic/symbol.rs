use std::collections::{HashMap, VecDeque};

use super::types::TypeEnum;

#[derive(Debug)]
pub struct Symbol {
    pub name: String,
    pub ty: TypeEnum,
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolID(usize);

#[derive(Default, Debug)]
pub struct SymbolTable {
    pub symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn push_scope(&mut self) {
        self.scope_stack.push_back(Default::default());
    }

    pub fn pop_scope(&mut self) -> HashMap<String, SymbolID> {
        self.scope_stack.pop_back().expect("Popped too many stacks")
    }

    pub fn push_local_symbol(&mut self, symbol: Symbol) -> SymbolID {
        self.symbols.push(symbol);
        let symbol_name = symbol.name.clone();
        let symbol_id = SymbolID(self.symbols.len() - 1);
        self.scope_stack
            .back()
            .expect("Where scope?")
            .insert(symbol_name, symbol_id);
        symbol_id
    }

    pub fn get_symbol_id_by_name(&self, name: &str) -> Option<SymbolID> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(*symbol);
            }
        }
        None
    }

    pub fn get_symbol_by_name(&self, name: &str) -> Option<&Symbol> {
        let symbol_id = self.get_symbol_id_by_name(name)?;
        self.symbols.get(symbol_id.0)
    }
}

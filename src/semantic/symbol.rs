use std::collections::{HashMap, VecDeque};

use super::types::TypeEnum;

#[derive(Debug)]
pub struct Symbol {
    pub identifier: String,
    pub ty: TypeEnum,
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolID(usize);

#[derive(Default, Debug)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    // symbol_lookup: HashMap<String, SymbolID>,
}

impl SymbolTable {
    // pub fn push_scope(&mut self) {
    //     self.scope_stack.push_back(Default::default());
    // }
    //
    // pub fn pop_scope(&mut self) -> HashMap<String, SymbolID> {
    //     self.scope_stack.pop_back().expect("Popped too many stacks")
    // }

    pub fn push_local_symbol(&mut self, symbol: Symbol) -> SymbolID {
        // let name = symbol.identifier.clone();
        self.symbols.push(symbol);
        let symbol_id = SymbolID(self.symbols.len() - 1);
        // self.symbol_lookup.insert(name, symbol_id);
        symbol_id
    }

    // pub fn get_symbol_id_by_name(&self, name: &str) -> Option<SymbolID> {
    //     self.symbol_lookup.get(name).copied()
    // }
    //
    // pub fn get_symbol_by_name(&self, name: &str) -> Option<&Symbol> {
    //     let symbol_id = self.get_symbol_id_by_name(name)?;
    //     self.symbols.get(symbol_id.0)
    // }

    pub fn get_symbol(&self, id: SymbolID) -> &Symbol {
        self.symbols.get(id.0).expect("Unexpected SymbolID")
    }
}

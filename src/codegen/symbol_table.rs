use std::collections::{HashMap, VecDeque};

use inkwell::{
    types::BasicTypeEnum,
    values::{FunctionValue, PointerValue},
};

#[derive(Copy, Clone)]
pub struct Symbol<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub ty: BasicTypeEnum<'ctx>,
}

#[derive(Default)]
pub struct SymbolTable<'ctx> {
    pub scope_stack: VecDeque<HashMap<String, Symbol<'ctx>>>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn push_value(&mut self, name: &str, symbol: Symbol<'ctx>) {
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

    pub fn get_value(&self, name: &str) -> Option<Symbol<'ctx>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(*value);
            }
        }
        None
    }

    pub fn add_function(&mut self, name: String, function: FunctionValue<'ctx>) {
        self.functions.insert(name, function);
    }

    pub fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.functions.get(name).copied()
    }
}

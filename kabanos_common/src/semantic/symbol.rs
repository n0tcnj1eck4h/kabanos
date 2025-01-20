use std::collections::HashMap;

use super::{error::SemanticError, types::Type, FunctionDeclaration, FunctionDefinition};

#[derive(Debug)]
pub struct Variable {
    pub identifier: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableID(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionID(usize);

#[derive(Default, Debug)]
pub struct SymbolTable {
    variables: Vec<Variable>,
    fn_decls: Vec<FunctionDeclaration>,
    fn_defs: HashMap<FunctionID, FunctionDefinition>,
    fn_lookup: HashMap<String, FunctionID>,
}

impl SymbolTable {
    pub fn add_variable(&mut self, symbol: Variable) -> VariableID {
        self.variables.push(symbol);
        VariableID(self.variables.len() - 1)
    }

    pub fn declare_function(
        &mut self,
        fn_decl: FunctionDeclaration,
    ) -> Result<FunctionID, SemanticError> {
        let existing_id = self.get_function_id_by_decl(&fn_decl)?;
        Ok(existing_id.unwrap_or_else(|| {
            let name = fn_decl.name.clone();
            self.fn_decls.push(fn_decl);
            let id = FunctionID(self.fn_decls.len() - 1);
            self.fn_lookup.insert(name, id);
            id
        }))
    }

    pub fn iterate_functions(&self) -> impl Iterator<Item = FunctionID> {
        (0..self.fn_decls.len()).map(FunctionID)
    }

    pub fn get_function_body(&self, fn_id: FunctionID) -> Option<&FunctionDefinition> {
        self.fn_defs.get(&fn_id)
    }

    pub fn pop_function_body(&mut self, fn_id: FunctionID) -> Option<FunctionDefinition> {
        self.fn_defs.remove(&fn_id)
    }

    pub fn get_function_id_by_decl(
        &self,
        decl: &FunctionDeclaration,
    ) -> Result<Option<FunctionID>, SemanticError> {
        let name = &decl.name;
        let Some(my_decl_id) = self.fn_lookup.get(name) else {
            return Ok(None);
        };

        let my_decl = self.fn_decls.get(my_decl_id.0).unwrap();
        if my_decl != decl {
            return Err(SemanticError::SignatureMismatch);
        }

        Ok(Some(*my_decl_id))
    }

    pub fn get_function_id_by_name(&self, name: &str) -> Option<FunctionID> {
        Some(*self.fn_lookup.get(name)?)
    }

    pub fn define_function(
        &mut self,
        id: FunctionID,
        definition: FunctionDefinition,
    ) -> Result<(), SemanticError> {
        if self.fn_defs.contains_key(&id) {
            return Err(SemanticError::FunctionRedefiniton);
        }
        self.fn_defs.insert(id, definition);
        Ok(())
    }

    pub fn get_function(&self, id: FunctionID) -> &FunctionDeclaration {
        self.fn_decls.get(id.0).expect("Unexpected FunctionID")
    }

    pub fn get_variable(&self, id: VariableID) -> &Variable {
        self.variables.get(id.0).expect("Unexpected VariableID")
    }
}

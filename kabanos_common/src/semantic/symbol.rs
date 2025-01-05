use std::collections::HashMap;

use super::{error::SemanticError, types::TypeKind, FunctionDeclaration, FunctionDefinition};

#[derive(Debug)]
pub struct Variable {
    pub identifier: String,
    pub ty: TypeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableID(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionID(usize);

#[derive(Default, Debug)]
pub struct SymbolTable {
    pub variables: Vec<Variable>,
    pub function_decls: Vec<FunctionDeclaration>,
    pub function_defs: HashMap<FunctionID, FunctionDefinition>,
    pub function_lookup: HashMap<String, FunctionID>,
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
            self.function_decls.push(fn_decl);
            let id = FunctionID(self.function_decls.len() - 1);
            self.function_lookup.insert(name, id);
            id
        }))
    }

    pub fn iterate_functions(&self) -> impl Iterator<Item = FunctionID> {
        (0..self.function_decls.len()).map(FunctionID)
    }

    pub fn get_function_body(&self, fn_id: FunctionID) -> Option<&FunctionDefinition> {
        self.function_defs.get(&fn_id)
    }

    pub fn pop_function_body(&mut self, fn_id: FunctionID) -> Option<FunctionDefinition> {
        self.function_defs.remove(&fn_id)
    }

    pub fn get_function_id_by_decl(
        &self,
        decl: &FunctionDeclaration,
    ) -> Result<Option<FunctionID>, SemanticError> {
        let name = &decl.name;
        let Some(my_decl_id) = self.function_lookup.get(name) else {
            return Ok(None);
        };

        let my_decl = self.function_decls.get(my_decl_id.0).unwrap();
        if my_decl != decl {
            return Err(SemanticError::SignatureMismatch);
        }

        Ok(Some(*my_decl_id))
    }

    pub fn get_function_id_by_name(&self, name: &str) -> Option<FunctionID> {
        Some(*self.function_lookup.get(name)?)
    }

    pub fn define_function(
        &mut self,
        id: FunctionID,
        definition: FunctionDefinition,
    ) -> Result<(), SemanticError> {
        if self.function_defs.contains_key(&id) {
            return Err(SemanticError::FunctionRedefiniton);
        }
        self.function_defs.insert(id, definition);
        Ok(())
    }

    pub fn get_function(&self, id: FunctionID) -> &FunctionDeclaration {
        self.function_decls
            .get(id.0)
            .expect("Unexpected FunctionID")
    }

    pub fn get_variable(&self, id: VariableID) -> &Variable {
        self.variables.get(id.0).expect("Unexpected VariableID")
    }
}

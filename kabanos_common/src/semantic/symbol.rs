use std::collections::HashMap;

use super::{error::SemanticError, types::TypeKind, FunctionDeclaration, Statement};

#[derive(Debug)]
pub struct Variable {
    pub identifier: String,
    pub ty: TypeKind,
}

#[derive(Debug, Clone, Copy)]
pub struct VariableID(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionID(usize);

#[derive(Default, Debug)]
pub struct SymbolTable {
    variables: Vec<Variable>,
    function_decls: Vec<FunctionDeclaration>,
    function_defs: HashMap<FunctionID, Vec<Statement>>,
    function_lookup: HashMap<String, FunctionID>,
}

impl SymbolTable {
    pub fn push_local_var(&mut self, symbol: Variable) -> VariableID {
        self.variables.push(symbol);
        VariableID(self.variables.len() - 1)
    }

    pub fn declare_function(
        &mut self,
        fn_decl: FunctionDeclaration,
    ) -> Result<FunctionID, SemanticError> {
        let existing_id = self.get_function_id_by_decl(&fn_decl)?;
        Ok(existing_id.unwrap_or_else(|| {
            self.function_decls.push(fn_decl);
            FunctionID(self.function_decls.len() - 1)
        }))
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

        return Ok(Some(*my_decl_id));
    }

    pub fn get_function_id_by_name(&self, name: &str) -> Option<FunctionID> {
        Some(*self.function_lookup.get(name)?)
    }

    pub fn define_function(
        &mut self,
        id: FunctionID,
        body: Vec<Statement>,
    ) -> Result<(), SemanticError> {
        if self.function_defs.contains_key(&id) {
            return Err(SemanticError::FunctionRedefiniton);
        }
        self.function_defs.insert(id, body);
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

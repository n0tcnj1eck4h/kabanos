use std::collections::HashMap;

use crate::ast::Statement;

#[derive(Debug)]
pub struct FunctionID(usize);

#[derive(Debug)]
pub struct ModuleID(usize);

#[derive(Debug)]
pub struct TypeID(usize);

#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
    pub function_map: HashMap<String, FunctionID>,
}

#[derive(Debug)]
pub enum Type {
    Struct(Vec<(String, TypeID)>),
    Union(Vec<(String, TypeID)>),
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
}

#[derive(Debug)]
pub struct FunctionParam {
    name: String,
    datatype: TypeID,
}

#[derive(Debug)]
pub enum Function {
    Extern {
        name: String,
        params: Vec<FunctionParam>,
        calling_convention: String,
    },
    Local {
        name: String,
        params: Vec<FunctionParam>,
        return_type: Option<Type>,
        body: Vec<Statement>,
    },
}

#[derive(Debug)]
pub struct Program {
    pub modules: Vec<Module>,
    pub module_map: HashMap<String, ModuleID>,
    pub struct_types: Vec<Type>,
}

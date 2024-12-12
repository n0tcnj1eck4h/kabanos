pub mod error;
pub mod expression;
pub mod from_ast;
pub mod operator;
pub mod primitive;
pub mod symbol;
pub mod types;

use std::collections::HashMap;

use expression::Expression;
use symbol::LocalVarID;
use types::TypeKind;

#[derive(Debug)]
pub struct Scope {
    pub symbol: LocalVarID,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Module {
    pub function_defs: HashMap<String, FunctionDefinition>,
    pub function_decls: HashMap<String, FunctionDeclaration>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<Parameter>,
    pub ty: Option<TypeKind>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub declaration: FunctionDeclaration,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub ty: TypeKind,
}

#[derive(Debug)]
pub enum Statement {
    Conditional(Expression, Vec<Statement>, Option<Vec<Statement>>),
    VoidFunctionCall(String, Vec<Expression>),
    Loop(Expression, Vec<Statement>),
    Return(Option<Expression>),
    Expression(Expression),
    Block(Scope),
}

pub mod error;
pub mod expression;
pub mod expression_builder;
pub mod module_builder;
pub mod operator;
pub mod statement_builder;
pub mod symbol;
pub mod types;

use expression::Expression;
use symbol::{FunctionID, SymbolTable, VariableID};
use types::Type;

#[derive(Default, Debug)]
pub struct Module {
    pub symbol_table: SymbolTable,
}

#[derive(Debug)]
pub struct Scope {
    pub variable_id: VariableID,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParam {
    pub identifier: String,
    pub ty: Type,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub body: Vec<Statement>,
    pub params: Vec<VariableID>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub id: FunctionID,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub enum Statement {
    Conditional(Expression, Vec<Statement>, Option<Vec<Statement>>),
    VoidFunctionCall(FunctionCall),
    Loop(Expression, Vec<Statement>),
    Return(Option<Expression>),
    Expression(Expression),
    Block(Scope),
}

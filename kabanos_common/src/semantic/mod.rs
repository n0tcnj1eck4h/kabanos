pub mod error;
pub mod expression;
pub mod expression_builder;
pub mod module_builder;
pub mod operator;
pub mod primitive;
pub mod statement_builder;
pub mod symbol;
pub mod types;

use expression::Expression;
use symbol::{SymbolTable, VariableID};
use types::TypeKind;

#[derive(Default, Debug)]
pub struct Module {
    pub symbol_table: SymbolTable,
}

#[derive(Debug)]
pub struct Scope {
    pub symbol: VariableID,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<VariableID>,
    pub ty: Option<TypeKind>,
}

#[derive(Debug)]
pub enum Statement {
    Conditional(Expression, Vec<Statement>, Option<Vec<Statement>>),
    VoidFunctionCall(FunctionDeclaration, Vec<Expression>),
    Loop(Expression, Vec<Statement>),
    Return(Option<Expression>),
    Expression(Expression),
    Block(Scope),
}

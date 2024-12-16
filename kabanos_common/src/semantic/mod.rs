pub mod error;
pub mod expression;
pub mod expression_builder;
pub mod from_ast;
pub mod operator;
pub mod primitive;
pub mod symbol;
pub mod types;

use expression::Expression;
use symbol::VariableID;
use types::TypeKind;

#[derive(Debug)]
pub struct Scope {
    pub symbol: VariableID,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<Parameter>,
    pub ty: Option<TypeKind>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub ty: TypeKind,
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

pub mod error;
pub mod expression;
pub mod from_ast;
pub mod operator;
pub mod primitive;
pub mod symbol;
pub mod types;

use expression::{ExpressionEnum, IntExpression};
use symbol::{SymbolID, SymbolTable};
use types::TypeEnum;

#[derive(Debug, Default)]
pub struct Scope {
    pub local_symbols: Vec<SymbolID>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<FunctionDeclaration>,
    pub functions: Vec<FunctionDefinition>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<Parameter>,
    pub ty: Option<TypeEnum>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub declaration: FunctionDeclaration,
    pub body: Scope,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub ty: TypeEnum,
}

#[derive(Debug)]
pub enum Statement {
    Conditional(IntExpression, Box<Statement>, Option<Box<Statement>>),
    VoidFunctionCall(String, Vec<ExpressionEnum>),
    Loop(IntExpression, Box<Statement>),
    Return(Option<ExpressionEnum>),
    Expression(ExpressionEnum),
    Block(Scope),
}

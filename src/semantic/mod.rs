mod error;
mod from_ast;
mod operator;
mod primitive;

use std::str::FromStr;

pub use error::*;
pub use operator::*;
pub use primitive::*;

use crate::token::Operator;

#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<FunctionDeclaration>,
    pub functions: Vec<FunctionDefinition>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<Parameter>,
    pub ty: Option<Primitive>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub declaration: FunctionDeclaration,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Parameter {
    pub ty: Primitive,
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum LValue {
    Identifier(String),
    // StructField,
    // PointerDereference,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    LocalVar(String, Primitive, Option<Expression>),
    Loop(Expression, Box<Statement>),
    Expression(Expression),
    Block(Vec<Statement>),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(u64),
    BooleanLiteral(bool),
    FloatLiteral(f64),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    Assignment(LValue, Box<Expression>),
    LValue(LValue),
}

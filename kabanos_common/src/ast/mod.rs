pub mod error;
pub mod parser;

use crate::{span::Span, token::Operator};

#[derive(Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    pub fn_declarations: Vec<FunctionPrototype>,
    pub fn_definitions: Vec<FunctionDefinition>,
    pub ty_definitions: Vec<Composite>,
    pub globals: Vec<GlobalVariableDefintion>,
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub calling_convention: Option<String>,
    pub return_type: Option<String>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub prototype: FunctionPrototype,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct GlobalVariableDefintion {
    pub datatype: String,
    pub name: String,
}

#[derive(Debug)]
pub struct Import {
    pub path: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub ty: String,
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    LocalVar(String, Option<String>, Option<Expression>),
    Loop(Expression, Box<Statement>),
    Expression(Expression),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Identifier(String),
    BinaryOp(Box<Expression>, Operator, Box<Expression>),
    UnaryOperation(Operator, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
}

impl ExpressionKind {
    pub fn is_strongly_typed(&self) -> bool {
        match self {
            ExpressionKind::IntegerLiteral(_)
            | ExpressionKind::FloatLiteral(_)
            | ExpressionKind::BooleanLiteral(_)
            | ExpressionKind::BinaryOp(_, _, _)
            | ExpressionKind::UnaryOperation(_, _) => false,

            ExpressionKind::FunctionCall(_, _)
            | ExpressionKind::Identifier(_)
            | ExpressionKind::StringLiteral(_) => true,
        }
    }
}

#[derive(Debug)]
pub struct CompositeField {
    pub name: String,
    pub datatype: String,
}

#[derive(Debug)]
pub struct Composite {
    pub name: String,
    pub fields: Vec<CompositeField>,
}

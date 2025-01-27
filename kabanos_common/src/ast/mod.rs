pub mod error;
pub mod parser;

use crate::{
    span::{Span, Spanned},
    token::Operator,
};

#[derive(Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    pub fn_decls: Vec<FunctionPrototype>,
    pub fn_defs: Vec<FunctionDefinition>,
    pub ty_defs: Vec<Composite>,
    pub globals: Vec<GlobalVariableDefintion>,
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub calling_convention: Option<String>,
    pub return_type: Option<Type>,
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
    pub ty: Type,
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Conditional(Spanned<Expression>, Box<Statement>, Option<Box<Statement>>),
    LocalVar(
        Spanned<String>,
        Option<Spanned<Type>>,
        Option<Spanned<Expression>>,
    ),
    Loop(Spanned<Expression>, Box<Statement>),
    Expression(Spanned<Expression>),
    Return(Option<Spanned<Expression>>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Identifier(String),
    BinaryOp(Box<Spanned<Expression>>, Operator, Box<Spanned<Expression>>),
    UnaryOperation(Operator, Box<Spanned<Expression>>),
    FunctionCall(String, Vec<Spanned<Expression>>),
    Cast(Box<Spanned<Expression>>, Type),
}

impl Expression {
    pub fn is_strongly_typed(&self) -> bool {
        match self {
            Expression::IntegerLiteral(_) | Expression::FloatLiteral(_) => false,

            Expression::UnaryOperation(_, expr) => expr.is_strongly_typed(),
            Expression::BinaryOp(l, _, r) => l.is_strongly_typed() || r.is_strongly_typed(),

            Expression::FunctionCall(..)
            | Expression::Cast(..)
            | Expression::BooleanLiteral(_)
            | Expression::Identifier(_)
            | Expression::StringLiteral(_) => true,
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

#[derive(Debug, Clone)]
pub struct Type {
    pub name: String,
    pub pointers: u32,
}

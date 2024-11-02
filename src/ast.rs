use crate::token::Operator;

#[derive(Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    pub function_declarations: Vec<FunctionPrototype>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub type_definitions: Vec<Composite>,
    pub globals: Vec<GlobalVariableDefintion>,
}

#[derive(Debug)]
pub struct FunctionPrototype {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub calling_convention: Option<String>,
    pub return_type: Option<String>,
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

#[derive(Debug)]
pub struct Parameter {
    pub ty: String,
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Conditional(Expression, Vec<Statement>, Option<Vec<Statement>>),
    LocalVar(String, Option<String>, Option<Expression>),
    Loop(Expression, Vec<Statement>),
    Expression(Expression),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(u64),
    FloatingPointLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Identifier(String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
    UnaryOperation(Operator, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
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

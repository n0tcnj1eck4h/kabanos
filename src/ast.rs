use crate::token::Operator;

#[derive(Debug)]
pub struct Program {
    pub imports: Vec<Import>,
    pub function_declarations: Vec<FunctionDeclaration>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub globals: Vec<GlobalVariableDefintion>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Statement,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<String>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub calling_convention: Option<String>,
    pub return_type: Option<String>,
}

#[derive(Debug)]
pub struct GlobalVariableDefintion {
    pub global_type: String,
    pub name: String,
}

#[derive(Debug)]
pub struct Import {
    pub path: Vec<String>,
}

#[derive(Debug)]
pub struct Parameter {
    pub param_type: String,
    pub name: String,
}

#[derive(Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    LocalVar(String, String),
    Loop(Expression, Box<Statement>),
    Assignment(String, Expression),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral(i128),
    FloatingPointLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Identifier(String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
    UnaryOperation(Operator, Box<Expression>),
}

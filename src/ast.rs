use crate::token::{Operator, Type};

#[derive(Debug)]
pub struct Program {
    pub imports: Vec<String>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub globals: Vec<GlobalVariableDefintion>,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Statement,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
}

#[derive(Debug)]
pub struct GlobalVariableDefintion {
    pub type_: Type,
    pub name: String,
}

#[derive(Debug)]
pub struct Import {
    pub path: String,
}

#[derive(Debug)]
pub struct Parameter {
    pub type_: Type,
    pub name: String,
}

#[derive(Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    LocalVar(Type, String),
}

impl Statement {
    pub fn pretty_print(&self, depth: usize) {
        for i in 0..depth {
            if i == depth - 1 {
                print!("├─ ");
            } else {
                print!("│  ");
            }
        }

        match self {
            Statement::Block(statements) => {
                println!("block");
                for s in statements.iter() {
                    s.pretty_print(depth + 1);
                }
            }
            Statement::LocalVar(..) => println!("local"),
            Statement::Conditional(..) => println!("conditioanl"),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral(i128),
    FloatingPointLiteral(f64),
    Identifier(String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
    UnaryOperation(Operator, Box<Expression>),
}

pub mod error;
pub mod operator;
pub mod primitive;

use error::SemanticError;
use operator::{BinaryOperator, UnaryOperator};
use primitive::Primitive;

use crate::ast;

#[derive(Debug)]
pub struct Module {
    pub functions: Vec<FunctionDefinition>,
}

impl TryFrom<ast::Module> for Module {
    type Error = SemanticError;

    fn try_from(value: ast::Module) -> Result<Self, Self::Error> {
        let mut functions = Vec::new();
        for s in value.function_definitions {
            let s = s.try_into()?;
            functions.push(s);
        }

        Ok(Self { functions })
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Vec<Statement>,
    pub params: Vec<Parameter>,
    pub ty: Option<Primitive>,
}

impl TryFrom<ast::FunctionDefinition> for FunctionDefinition {
    type Error = SemanticError;
    fn try_from(value: ast::FunctionDefinition) -> Result<Self, Self::Error> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub ty: Primitive,
    pub name: String,
}

impl TryFrom<ast::Parameter> for Parameter {
    type Error = SemanticError;
    fn try_from(value: ast::Parameter) -> Result<Self, Self::Error> {
        let name = value.name;
        let ty = value.ty.parse()?;
        Ok(Self { ty, name })
    }
}

#[derive(Debug, Clone)]
pub enum LValue {
    Identifier(String),
    StructField,
    PointerDereference,
}

impl TryFrom<ast::Expression> for LValue {
    type Error = SemanticError;
    fn try_from(value: ast::Expression) -> Result<Self, Self::Error> {
        match value {
            ast::Expression::Identifier(ident) => Ok(Self::Identifier(ident)),
            _ => Err(SemanticError::LValue(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    LocalVar(String, Primitive, Option<Expression>),
    Loop(Expression, Box<Statement>),
    Expression(Expression),
    Block(Vec<Statement>),
    Return(Expression),
}

impl TryFrom<ast::Statement> for Statement {
    type Error = SemanticError;
    fn try_from(value: ast::Statement) -> Result<Self, Self::Error> {
        match value {
            ast::Statement::Conditional(expr, true_block, else_block) => {
                Ok(Statement::Conditional(
                    expr.try_into()?,
                    Box::new((*true_block).try_into()?),
                    else_block
                        .map(|eb| (*eb).try_into())
                        .transpose()?
                        .map(Box::new),
                ))
            }
            ast::Statement::Expression(expr) => Ok(Statement::Expression(expr.try_into()?)),
            ast::Statement::Loop(expr, statement) => Ok(Statement::Loop(
                expr.try_into()?,
                Box::new((*statement).try_into()?),
            )),
            ast::Statement::Block(statements) => {
                let mut r = Vec::new();
                for s in statements {
                    let s = s.try_into()?;
                    r.push(s);
                }
                Ok(Self::Block(r))
            }
            ast::Statement::Return(expr) => Ok(Self::Return(expr.try_into()?)),
            ast::Statement::LocalVar(identifier, ty, expr) => {
                if let Some(ty) = ty {
                    Ok(Self::LocalVar(
                        identifier,
                        ty.parse()?,
                        expr.map(TryInto::try_into).transpose()?,
                    ))
                } else {
                    Err(SemanticError::MissingExplicitType)
                }
            }
        }
    }
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

impl TryFrom<ast::Expression> for Expression {
    type Error = SemanticError;
    fn try_from(value: ast::Expression) -> Result<Self, Self::Error> {
        todo!()
    }
}

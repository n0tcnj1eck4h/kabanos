mod error;
mod operator;
mod primitive;

use std::str::FromStr;

pub use error::*;
pub use operator::*;
pub use primitive::*;

use crate::{ast, token::Operator};

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
        let mut body = Vec::new();
        for statement in value.body {
            let statement = statement.try_into()?;
            body.push(statement);
        }

        let mut params = Vec::new();
        for param in value.parameters {
            let param = param.try_into()?;
            params.push(param);
        }

        let ty = value
            .return_type
            .map(|ty| FromStr::from_str(&ty))
            .transpose()?;

        Ok(Self {
            name: value.name,
            body,
            params,
            ty,
        })
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
    // StructField,
    // PointerDereference,
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
        match value {
            ast::Expression::Identifier(ident) => Ok(Self::LValue(LValue::Identifier(ident))),
            ast::Expression::FunctionCall(_, _) => unimplemented!(),
            ast::Expression::StringLiteral(_) => unimplemented!(),
            ast::Expression::IntegerLiteral(int) => Ok(Self::IntegerLiteral(int)),
            ast::Expression::BooleanLiteral(bool) => Ok(Self::BooleanLiteral(bool)),
            ast::Expression::FloatingPointLiteral(float) => Ok(Self::FloatLiteral(float)),
            ast::Expression::UnaryOperation(op, expr) => {
                let op = op.try_into()?;
                let expr = Box::new((*expr).try_into()?);
                Ok(Self::UnaryOperation(op, expr))
            }
            ast::Expression::BinaryOperation(l, op, r) => {
                if op == Operator::Assign {
                    let l = (*l).try_into()?;
                    let r = Box::new((*r).try_into()?);
                    Ok(Self::Assignment(l, r))
                } else {
                    let op = op.try_into()?;
                    let l = Box::new((*l).try_into()?);
                    let r = Box::new((*r).try_into()?);
                    Ok(Self::BinaryOperation(l, op, r))
                }
            }
        }
    }
}

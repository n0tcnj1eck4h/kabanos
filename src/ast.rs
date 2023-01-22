use crate::{environment::Environment, operator::Operator, value::Value};

#[derive(Debug)]
pub enum Error {
    BinaryOperationError(Value, Operator, Value),
    UndefinedIdentifierError(String)
}

pub trait Evaluate {
    fn evaluate(&self, env: &Environment) -> Result<Value, Error>;
}

pub enum ExpressionAST {
    Literal(Value),
    Variable(String),
    BinaryOperation(Box<ExpressionAST>, Operator, Box<ExpressionAST>),
}

impl Evaluate for ExpressionAST {
    fn evaluate(&self, env: &Environment) -> Result<Value, Error> {
        match self {
            Self::Literal(val) => Ok(val.to_owned()),
            Self::BinaryOperation(lhs, op, rhs) => {
                let lhs = lhs.evaluate(env)?;
                let rhs = rhs.evaluate(env)?;

                Err(Error::BinaryOperationError(lhs, op.to_owned(), rhs))
            },
            Self::Variable(var) => {
                Err(Error::UndefinedIdentifierError(var.to_owned()))
            }
        }
    }
}

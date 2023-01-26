use crate::{environment::Environment, operator::Operator, value::Value};

#[derive(Debug)]
pub enum Error {
    BinaryOperationError(Value, Operator, Value),
    UnaryOperationError(Operator, Value),
    UndefinedIdentifierError(String),
    BooleanExpectedError(Value),
}

pub trait Evaluate {
    fn evaluate(&self, env: &Environment) -> Result<Value, Error>;
}

#[derive(Debug)]
pub enum ExpressionAST {
    Literal(Value),
    Identifier(String),
    BinaryOperation(Box<ExpressionAST>, Operator, Box<ExpressionAST>),
    UnaryOperation(Operator, Box<ExpressionAST>),
}

impl Evaluate for ExpressionAST {
    fn evaluate(&self, env: &Environment) -> Result<Value, Error> {
        match self {
            Self::Literal(val) => Ok(val.to_owned()),
            Self::BinaryOperation(lhs, op, rhs) => {
                let lhs = lhs.evaluate(env)?;
                let rhs = rhs.evaluate(env)?;

                if let (Value::Integer(l), Value::Integer(r)) = (&lhs, &rhs) {
                    match op {
                        Operator::Power          => Ok(Value::Integer(l.pow(r.to_owned().unsigned_abs()))), // TODO
                        Operator::Add            => Ok(Value::Integer(l + r)),
                        Operator::Subtract       => Ok(Value::Integer(l - r)),
                        Operator::Multiply       => Ok(Value::Integer(l * r)),
                        Operator::Divide         => Ok(Value::Integer(l / r)),
                        Operator::Modulo         => Ok(Value::Integer(l % r)),
                        Operator::BinaryAnd      => Ok(Value::Integer(l & r)),
                        Operator::BinaryOr       => Ok(Value::Integer(l | r)),
                        Operator::BinaryXor      => Ok(Value::Integer(l ^ r)),
                        Operator::BinaryRight    => Ok(Value::Integer(l >> r)),
                        Operator::BinaryLeft     => Ok(Value::Integer(l << r)),
                        Operator::Equal          => Ok(Value::Boolean(l == r)),
                        Operator::NotEqual       => Ok(Value::Boolean(l != r)),
                        Operator::GreaterOrEqual => Ok(Value::Boolean(l >= r)),
                        Operator::LessOrEqual    => Ok(Value::Boolean(l <= r)),
                        Operator::LogicAnd       => Ok(Value::Boolean(*l != 0 && *r != 0)),
                        Operator::LogicOr        => Ok(Value::Boolean(*l != 0 || *r != 0)),
                        Operator::Less           => Ok(Value::Boolean(l < r)),
                        Operator::Greater        => Ok(Value::Boolean(l > r)),
                        _ => Err(Error::BinaryOperationError(lhs, op.to_owned(), rhs)),
                    }
                } else {
                    Err(Error::BinaryOperationError(lhs, op.to_owned(), rhs))
                }
            }
            Self::Identifier(var) => Err(Error::UndefinedIdentifierError(var.to_owned())),
            Self::UnaryOperation(op, rhs) =>  {
                let rhs = rhs.evaluate(env)?;

                if let Value::Integer(r) = &rhs {
                    match op {
                        Operator::Subtract  => Ok(Value::Integer(-r)),
                        Operator::BinaryNot => Ok(Value::Integer(!r)),
                        Operator::LogicNot  => Ok(Value::Boolean(*r != 0)),
                        _ =>  Err(Error::UnaryOperationError(*op, rhs))
                    }
                } else if let Value::Boolean(b) = &rhs {
                    match op {
                        Operator::BinaryNot => Ok(Value::Integer(!b as i32)),
                        Operator::LogicNot  => Ok(Value::Boolean(!b)),
                        _ =>  Err(Error::UnaryOperationError(*op, rhs))
                    }
                } else {
                    Err(Error::UnaryOperationError(*op, rhs))
                }
            }       
        }
    }
}

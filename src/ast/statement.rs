use crate::{environment::Environment, value::Value};

use super::expression::{ExpressionAST, Evaluate, Error};

#[derive(Debug)]
pub enum StatementAST {
    Print(Box<ExpressionAST>),
    Block(Vec<Box<StatementAST>>),
    Conditional(Box<ExpressionAST>, Box<StatementAST>, Option<Box<StatementAST>>),
}

pub trait Execute {
    fn execute(&mut self, env: &Environment) -> Result<(), Error>;
}

impl Execute for StatementAST {
    fn execute(&mut self, env: &Environment) -> Result<(), Error> {
        match self {
            StatementAST::Print(expr) => println!("{:?}", expr.evaluate(env)),
            StatementAST::Block(statements) => for statement in statements {
                statement.execute(&env)?;
            },
            StatementAST::Conditional(condition, truth_branch, else_branch) => {
                let val = condition.evaluate(&env)?;
                if let Value::Boolean(truth) = &val {
                    if *truth {
                        truth_branch.execute(&env)?;
                    }
                    else if let Some(else_branch) = else_branch {
                        else_branch.execute(&env)?;
                    }
                }
                else {
                    return Err(Error::BooleanExpectedError(val));
                }
            }
        }

        Ok(())
    }
}

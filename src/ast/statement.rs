use crate::environment::Environment;

use super::expression::{ExpressionAST, Evaluate};

#[derive(Debug)]
pub enum StatementAST {
    Print(Box<ExpressionAST>),
    Block(Vec<Box<StatementAST>>)
}

pub trait Execute {
    fn execute(&mut self, env: &Environment) -> Result<(), ()>;
}

impl Execute for StatementAST {
    fn execute(&mut self, env: &Environment) -> Result<(), ()> {
        match self {
            StatementAST::Print(expr) => println!("{:?}", expr.evaluate(env)),
            StatementAST::Block(statements) => for statement in statements {
                statement.execute(&env)?;
            }
        }

        Ok(())
    }
}

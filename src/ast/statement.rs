use crate::environment::Environment;

use super::expression::{ExpressionAST, Evaluate};

#[derive(Debug)]
pub enum StatementAST {
    Print(Box<ExpressionAST>),
}

pub trait Execute {
    fn execute(&mut self, env: &Environment) -> Result<(), ()>;
}

impl Execute for StatementAST {
    fn execute(&mut self, env: &Environment) -> Result<(), ()> {
        match self {
            StatementAST::Print(expr) => println!("{:?}", expr.evaluate(env))
        }

        Ok(())
    }
}

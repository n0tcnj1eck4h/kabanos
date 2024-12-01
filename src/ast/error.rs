use std::fmt::Display;

use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedTokenError(Token),
    ExpressionExpectedError(Token),
    StatementExpectedError(Token),
    UnexpectedEOF,
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::UnexpectedTokenError(token) => {
                write!(f, "Unexpected {:?}", token.kind)
            }
            ParsingError::ExpressionExpectedError(token) => {
                write!(f, "Expression expected. Got {:?}", token.kind)
            }
            ParsingError::StatementExpectedError(token) => {
                write!(f, "Statement expected. Got {:?}", token.kind)
            }
            ParsingError::UnexpectedEOF => write!(f, "Unexpected end of file"),
        }
    }
}

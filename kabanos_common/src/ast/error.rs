use std::fmt::Display;

use crate::{span::Span, token::Token};

#[derive(Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedTokenError(Token),
    ExpressionExpectedError(Token),
    StatementExpectedError(Token),
    UnexpectedEOF(Span),
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::UnexpectedTokenError(token) => {
                write!(f, "Unexpected {}", token.kind)
            }
            ParsingError::ExpressionExpectedError(token) => {
                write!(f, "Expression expected. Got {}", token.kind)
            }
            ParsingError::StatementExpectedError(token) => {
                write!(f, "Statement expected. Got {}", token.kind)
            }
            ParsingError::UnexpectedEOF(_) => write!(f, "Unexpected end of file"),
        }
    }
}

impl ParsingError {
    pub fn get_span(&self) -> Span {
        match self {
            ParsingError::UnexpectedTokenError(token)
            | ParsingError::ExpressionExpectedError(token)
            | ParsingError::StatementExpectedError(token) => token.span,
            ParsingError::UnexpectedEOF(span) => *span,
        }
    }
}

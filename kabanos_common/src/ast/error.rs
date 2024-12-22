use std::fmt::Display;

use crate::{
    span::{HasSpan, Span, Spanned},
    token::Token,
};

#[derive(Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedTokenError(Spanned<Token>),
    ExpressionExpectedError(Spanned<Token>),
    StatementExpectedError(Spanned<Token>),
    UnexpectedEOF(Span),
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::UnexpectedTokenError(token) => {
                write!(f, "Unexpected {}", token)
            }
            ParsingError::ExpressionExpectedError(token) => {
                write!(f, "Expression expected. Got {}", token)
            }
            ParsingError::StatementExpectedError(token) => {
                write!(f, "Statement expected. Got {}", token)
            }
            ParsingError::UnexpectedEOF(_) => write!(f, "Unexpected end of file"),
        }
    }
}

impl HasSpan for ParsingError {
    fn get_span(&self) -> Span {
        match self {
            ParsingError::UnexpectedTokenError(token)
            | ParsingError::ExpressionExpectedError(token)
            | ParsingError::StatementExpectedError(token) => token.get_span(),
            ParsingError::UnexpectedEOF(span) => *span,
        }
    }
}

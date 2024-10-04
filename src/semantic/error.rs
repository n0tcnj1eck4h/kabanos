use std::fmt::Display;

use crate::{ast, token::Operator};

#[derive(Debug)]
pub enum SemanticError {
    NotABinOp(Operator),
    NotAUnaryOp(Operator),
    NotAPrimitive(String),
    LValue(ast::Expression),
    MissingExplicitType,
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotABinOp(op) => write!(f, "{:?} is not a binary operator", op),
            Self::NotAUnaryOp(op) => write!(f, "{:?} is not a unary operator", op),
            Self::NotAPrimitive(ident) => write!(f, "{:?} is not a valid primitive type", ident),
            Self::LValue(expr) => write!(f, "{:?} is not an lvalue", expr),
            Self::MissingExplicitType => write!(f, "Implicit variable types are not allowed yet"),
        }
    }
}

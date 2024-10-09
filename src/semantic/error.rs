use std::fmt::Display;

use crate::{ast, token::Operator};

use super::Primitive;

#[derive(Debug)]
pub enum SemanticError {
    NotBinOp(Operator),
    NotUnaryOp(Operator),
    NotPrimitive(String),
    LValue(ast::Expression),
    MissingExplicitType,
    VoidOperation,
    TypeMismatch {
        expected: Primitive,
        recieved: Option<Primitive>,
    },
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotBinOp(op) => write!(f, "{:?} is not a binary operator", op),
            Self::NotUnaryOp(op) => write!(f, "{:?} is not a unary operator", op),
            Self::NotPrimitive(ident) => write!(f, "{:?} is not a valid primitive type", ident),
            Self::LValue(expr) => write!(f, "{:?} is not an lvalue", expr),
            Self::MissingExplicitType => write!(f, "Implicit variable types are not allowed yet"),
            Self::VoidOperation => write!(f, "Operation an a void value"),
            Self::TypeMismatch { expected, recieved } => write!(
                f,
                "Mismatched types! {:?} expected, got {:?}",
                expected, recieved
            ),
        }
    }
}

use std::fmt::Display;

use crate::{ast, token::Operator};

use super::{operator::UnaryOperator, types::TypeKind};

#[derive(Debug)]
pub enum SemanticError {
    NotBinOp(Operator),
    NotUnaryOp(Operator),
    NotPrimitive(String),
    LValue(ast::Expression),
    VoidOperation,
    FunctionCallsNotImplemented,
    InvalidUnaryOp(UnaryOperator, TypeKind),
    Undeclared(String),
    ReturnTypeMismatch {
        expected: Option<TypeKind>,
        recieved: Option<TypeKind>,
    },
    NotLogic(TypeKind),
    TypeMismatch {
        expected: TypeKind,
        recieved: Option<TypeKind>,
    },
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticError::NotBinOp(op) => write!(f, "{:?} is not a binary operator", op),
            SemanticError::NotUnaryOp(op) => write!(f, "{:?} is not a unary operator", op),
            SemanticError::NotPrimitive(ident) => {
                write!(f, "{:?} is not a valid primitive type", ident)
            }
            SemanticError::LValue(expr) => write!(f, "{:?} is not an lvalue", expr),
            SemanticError::VoidOperation => write!(f, "Operation on a void value"),
            SemanticError::Undeclared(ident) => write!(f, "Unknown identifier {}", ident),
            SemanticError::TypeMismatch { expected, recieved } => write!(
                f,
                "Mismatched types! {:?} expected, got {:?}",
                expected, recieved
            ),
            SemanticError::NotLogic(type_kind) => write!(f, "{:?} is not a logic type", type_kind),
            SemanticError::ReturnTypeMismatch { expected, recieved } => write!(
                f,
                "Mismatched return types! {:?} expected, got {:?}",
                expected, recieved
            ),
            SemanticError::InvalidUnaryOp(unary_operator, type_kind) => write!(
                f,
                "Invalid unary operation {:?} on type {:?}",
                unary_operator, type_kind
            ),
            SemanticError::FunctionCallsNotImplemented => {
                write!(f, "Function calls are not implemented yet")
            }
        }
    }
}

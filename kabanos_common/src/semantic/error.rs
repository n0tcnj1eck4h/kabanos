use std::fmt::Display;

use crate::{ast, token::Operator};

use super::{operator::UnaryOperator, types::TypeKind};

#[derive(Debug)]
pub enum SemanticError {
    NotBinOp(Operator),
    NotUnaryOp(Operator),
    FunctionRedefinition(String),
    NotPrimitive(String),
    LValue(ast::Expression),
    VoidOperation,
    InvalidUnaryOp(UnaryOperator, TypeKind),
    Undeclared(String),
    WrongArgumentCount,
    ReturnTypeMismatch { expected: Option<TypeKind> },
    NotLogic(TypeKind),
    TypeMismatch { expected: TypeKind, found: TypeKind },
    FunctionRedefiniton,
    SignatureMismatch,
    InvalidBinOp,
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
            SemanticError::TypeMismatch { expected, found } => write!(
                f,
                "Mismatched types! {:?} expected, got {:?}",
                expected, found
            ),
            SemanticError::NotLogic(type_kind) => write!(f, "{:?} is not a logic type", type_kind),
            SemanticError::ReturnTypeMismatch { expected } => {
                write!(f, "Mismatched return types! {:?} expected", expected)
            }
            SemanticError::InvalidUnaryOp(unary_operator, type_kind) => write!(
                f,
                "Invalid unary operation {:?} on type {:?}",
                unary_operator, type_kind
            ),
            SemanticError::FunctionRedefinition(_) => {
                write!(f, "Function redefined")
            }
            SemanticError::WrongArgumentCount => {
                write!(f, "Argument count in function call is incorrect")
            }
            SemanticError::FunctionRedefiniton => write!(f, "Function redefined"),
            SemanticError::SignatureMismatch => write!(f, "Function signature mismatch"),
            SemanticError::InvalidBinOp => write!(f, "Invalid binary operation"),
        }
    }
}

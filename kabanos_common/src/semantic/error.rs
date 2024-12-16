use std::fmt::Display;

use crate::{span::Span, token::Operator};

use super::{expression::Expression, operator::UnaryOperator, types::TypeKind};

#[derive(Debug)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Span,
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)?;
        write!(f, " on line {}", self.span)
    }
}

#[derive(Debug)]
pub enum SemanticErrorKind {
    NotBinOp(Operator),
    NotUnaryOp(Operator),
    FunctionRedefinition(String),
    NotPrimitive(String),
    LValue(Expression),
    VoidOperation,
    InvalidUnaryOp(UnaryOperator, TypeKind),
    Undeclared(String),
    WrongArgumentCount,
    ReturnTypeMismatch { expected: Option<TypeKind> },
    TypeMismatch { expected: TypeKind, found: TypeKind },
    FunctionRedefiniton,
    SignatureMismatch,
    InvalidBinOp,
}

impl SemanticErrorKind {
    pub fn with_span(self, span: Span) -> SemanticError {
        SemanticError { kind: self, span }
    }
}

impl Display for SemanticErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticErrorKind::NotBinOp(op) => write!(f, "{:?} is not a binary operator", op),
            SemanticErrorKind::NotUnaryOp(op) => write!(f, "{:?} is not a unary operator", op),
            SemanticErrorKind::NotPrimitive(ident) => {
                write!(f, "{:?} is not a valid primitive type", ident)
            }
            SemanticErrorKind::LValue(expr) => write!(f, "{:?} is not an lvalue", expr),
            SemanticErrorKind::VoidOperation => write!(f, "Operation on a void value"),
            SemanticErrorKind::Undeclared(ident) => write!(f, "Unknown identifier {}", ident),
            SemanticErrorKind::TypeMismatch { expected, found } => write!(
                f,
                "Mismatched types! {:?} expected, got {:?}",
                expected, found
            ),
            SemanticErrorKind::ReturnTypeMismatch { expected } => {
                write!(f, "Mismatched return types! {:?} expected", expected)
            }
            SemanticErrorKind::InvalidUnaryOp(unary_operator, type_kind) => write!(
                f,
                "Invalid unary operation {:?} on type {:?}",
                unary_operator, type_kind
            ),
            SemanticErrorKind::FunctionRedefinition(_) => {
                write!(f, "Function redefined")
            }
            SemanticErrorKind::WrongArgumentCount => {
                write!(f, "Argument count in function call is incorrect")
            }
            SemanticErrorKind::FunctionRedefiniton => write!(f, "Function redefined"),
            SemanticErrorKind::SignatureMismatch => write!(f, "Function signature mismatch"),
            SemanticErrorKind::InvalidBinOp => write!(f, "Invalid binary operation"),
        }
    }
}

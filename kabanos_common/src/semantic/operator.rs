use crate::token::Operator;

use super::{error::SemanticError, types::Type};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOperator {
    Logic(LogicOp),
    Bitwise(BitwiseOp),
    Arithmetic(ArithmeticOp),
    Comparaison(ComparaisonOp),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LogicOp {
    LogicAnd,
    LogicOr,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BitwiseOp {
    BitAnd,
    BitOr,
    BitXor,
    BitLeft,
    BitRight,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ComparaisonOp {
    Equal,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    NotEqual,
}

impl TryFrom<Operator> for BinaryOperator {
    type Error = SemanticError;
    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        use BinaryOperator::*;
        Ok(match value {
            Operator::Equal => Comparaison(ComparaisonOp::Equal),
            Operator::Less => Comparaison(ComparaisonOp::Less),
            Operator::Greater => Comparaison(ComparaisonOp::Greater),
            Operator::LessOrEqual => Comparaison(ComparaisonOp::LessOrEqual),
            Operator::GreaterOrEqual => Comparaison(ComparaisonOp::GreaterOrEqual),
            Operator::NotEqual => Comparaison(ComparaisonOp::NotEqual),
            Operator::Add => Arithmetic(ArithmeticOp::Add),
            Operator::Minus => Arithmetic(ArithmeticOp::Subtract),
            Operator::Asterisk => Arithmetic(ArithmeticOp::Multiply),
            Operator::Divide => Arithmetic(ArithmeticOp::Divide),
            Operator::Modulo => Arithmetic(ArithmeticOp::Modulo),
            Operator::Ampersand => Bitwise(BitwiseOp::BitAnd),
            Operator::Pipe => Bitwise(BitwiseOp::BitOr),
            Operator::Caret => Bitwise(BitwiseOp::BitXor),
            Operator::LeftShift => Bitwise(BitwiseOp::BitLeft),
            Operator::RightShift => Bitwise(BitwiseOp::BitRight),
            Operator::LogicAnd => Logic(LogicOp::LogicAnd),
            Operator::LogicOr => Logic(LogicOp::LogicOr),
            _ => return Err(SemanticError::NotBinOp(value)),
        })
    }
}

impl BinaryOperator {
    pub fn get_result_ty(&self, operand_ty: Type) -> Result<Type, SemanticError> {
        Ok(match (self, operand_ty) {
            (BinaryOperator::Logic(_), Type::Bool) => Type::Bool,
            (BinaryOperator::Bitwise(_), ty @ Type::Int(_)) => ty,
            (BinaryOperator::Arithmetic(_), ty @ Type::Int(_)) => ty,
            (BinaryOperator::Arithmetic(_), ty @ Type::Float(_)) => ty,
            (BinaryOperator::Comparaison(_), _) => Type::Bool,
            _ => return Err(SemanticError::InvalidBinOp),
        })
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Negative,
    LogicNot,
    BitNot,
}

impl TryFrom<Operator> for UnaryOperator {
    type Error = SemanticError;
    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Minus => Ok(Self::Negative),
            Operator::Exclamation => Ok(Self::LogicNot),
            Operator::Tilde => Ok(Self::BitNot),
            _ => Err(SemanticError::NotUnaryOp(value)),
        }
    }
}

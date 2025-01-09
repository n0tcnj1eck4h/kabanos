use crate::token::Operator;

use super::error::SemanticError;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOperator {
    Logic(LogicOp),
    Bitwise(BitwiseOp),
    Arithmetic(ArithmeticOp),
    Comparaison(ComparaisonOp),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BitwiseOp {
    ShiftRight,
    ShiftLeft,
    And,
    Xor,
    Or,
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
            Operator::Plus => Arithmetic(ArithmeticOp::Add),
            Operator::Minus => Arithmetic(ArithmeticOp::Subtract),
            Operator::Asterisk => Arithmetic(ArithmeticOp::Multiply),
            Operator::Divide => Arithmetic(ArithmeticOp::Divide),
            Operator::Modulo => Arithmetic(ArithmeticOp::Modulo),
            Operator::Ampersand => Bitwise(BitwiseOp::And),
            Operator::Pipe => Bitwise(BitwiseOp::Or),
            Operator::Caret => Bitwise(BitwiseOp::Xor),
            Operator::LeftShift => Bitwise(BitwiseOp::ShiftLeft),
            Operator::RightShift => Bitwise(BitwiseOp::ShiftRight),
            Operator::LogicAnd => Logic(LogicOp::And),
            Operator::LogicOr => Logic(LogicOp::Or),
            _ => return Err(SemanticError::NotBinOp(value)),
        })
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Negative,
    LogicNot,
    BitNot,
    Deref,
    Ref,
}

impl TryFrom<Operator> for UnaryOperator {
    type Error = SemanticError;
    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Minus => Ok(Self::Negative),
            Operator::Exclamation => Ok(Self::LogicNot),
            Operator::Tilde => Ok(Self::BitNot),
            Operator::Asterisk => Ok(Self::Deref),
            Operator::Ampersand => Ok(Self::Ref),
            _ => Err(SemanticError::NotUnaryOp(value)),
        }
    }
}

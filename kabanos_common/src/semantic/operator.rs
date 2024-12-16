use crate::token::Operator;

use super::error::SemanticErrorKind;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Equal,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    NotEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitAnd,
    BitOr,
    BitXor,
    BitLeft,
    BitRight,
    LogicAnd,
    LogicOr,
    Assign,
}

impl TryFrom<Operator> for BinaryOperator {
    type Error = SemanticErrorKind;
    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Equal => Ok(Self::Equal),
            Operator::Less => Ok(Self::Less),
            Operator::Greater => Ok(Self::Greater),
            Operator::LessOrEqual => Ok(Self::LessOrEqual),
            Operator::GreaterOrEqual => Ok(Self::GreaterOrEqual),
            Operator::NotEqual => Ok(Self::NotEqual),
            Operator::Add => Ok(Self::Add),
            Operator::Minus => Ok(Self::Subtract),
            Operator::Asterisk => Ok(Self::Multiply),
            Operator::Divide => Ok(Self::Divide),
            Operator::Modulo => Ok(Self::Modulo),
            Operator::Ampersand => Ok(Self::BitAnd),
            Operator::Pipe => Ok(Self::BitOr),
            Operator::Caret => Ok(Self::BitXor),
            Operator::LeftShift => Ok(Self::BitLeft),
            Operator::RightShift => Ok(Self::BitRight),
            Operator::LogicAnd => Ok(Self::LogicAnd),
            Operator::LogicOr => Ok(Self::LogicOr),
            Operator::Assign => Ok(Self::Assign),
            _ => Err(SemanticErrorKind::NotBinOp(value)),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Negative,
    LogicNot,
    BitNot,
}

impl TryFrom<Operator> for UnaryOperator {
    type Error = SemanticErrorKind;
    fn try_from(value: Operator) -> Result<Self, Self::Error> {
        match value {
            Operator::Minus => Ok(Self::Negative),
            Operator::Exclamation => Ok(Self::LogicNot),
            Operator::Tilde => Ok(Self::BitNot),
            _ => Err(SemanticErrorKind::NotUnaryOp(value)),
        }
    }
}

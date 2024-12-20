use crate::span::Span;

use super::{
    operator::{BinaryOperator, UnaryOperator},
    symbol::FunctionID,
    types::TypeKind,
    VariableID,
};

#[derive(Debug, Clone)]
pub enum LValue {
    LocalVar(VariableID),
    // StructField,
    // PointerDereference,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub ty: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    LValue(LValue),
    IntegerLiteral(u64),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    UnaryOperation(UnaryOperator, Box<Expression>),
    FunctionCall(FunctionID, Vec<Expression>),
    Assignment(LValue, Box<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    Cast(Box<Expression>, TypeKind),
}

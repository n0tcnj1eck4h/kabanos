use super::{
    operator::{BinaryOperator, UnaryOperator},
    types::{FloatTy, IntTy, Type},
    FunctionCall, VariableID,
};

#[derive(Debug, Clone)]
pub enum LValue {
    LocalVar(VariableID),
    // StructField,
    // PointerDereference,
}

#[derive(Debug, Clone)]
pub enum Expression {
    LValue(LValue),
    BooleanLiteral(bool),
    IntegerLiteral(u64, IntTy),
    FloatLiteral(f64, FloatTy),
    StringLiteral(String),
    FunctionCall(FunctionCall),
    Assignment(LValue, Box<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    Cast(Box<Expression>, Type),
}

use super::{
    operator::{BinaryOperator, UnaryOperator},
    types::TypeKind,
    LocalVarID,
};

#[derive(Debug, Clone)]
pub enum LValue {
    LocalVar(LocalVarID),
    // StructField,
    // PointerDereference,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub ty: TypeKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    LValue(LValue),
    IntegerLiteral(u64),
    FloatLiteral(f64),
    UnaryOperation(UnaryOperator, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    Assignment(LValue, Box<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
}

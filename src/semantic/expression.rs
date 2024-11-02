use super::{
    operator::{BinaryOperator, UnaryOperator},
    types::{FloatType, IntegerType},
    SymbolID,
};

// #[derive(Debug, Clone)]
// pub enum LValue {
//     Identifier(SymbolID),
//     // StructField,
//     // PointerDereference,
// }

#[derive(Debug, Clone)]
pub enum IntExpression {
    LValue(SymbolID),
    IntegerLiteral(u64),
    BooleanLiteral(bool),
    UnaryOperation(UnaryOperator, Box<IntExpression>),
    FunctionCall(String, Vec<IntExpression>),
    Assignment(SymbolID, Box<IntExpression>),
    BinaryOperation(Box<IntExpression>, BinaryOperator, Box<IntExpression>),
    Cast {
        ty: IntegerType,
        expr: Box<ExpressionEnum>,
    },
}

#[derive(Debug, Clone)]
pub enum FloatExpression {
    FloatLiteral(f64),
    BinaryOperation(Box<FloatExpression>, BinaryOperator, Box<FloatExpression>),
    UnaryOperation(UnaryOperator, Box<FloatExpression>),
    FunctionCall(String, Vec<FloatExpression>),
    Assignment(SymbolID, Box<FloatExpression>),
    LValue(SymbolID),
    Cast {
        ty: FloatType,
        expr: Box<ExpressionEnum>,
    },
}

#[derive(Debug, Clone)]
pub enum ExpressionEnum {
    IntExpression(IntExpression),
    FloatExpression(FloatExpression),
}

use crate::token::Operator;

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral(i128),
    FloatingPointLiteral(f64),
    Identifier(String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
    UnaryOperation(Operator, Box<Expression>),
}

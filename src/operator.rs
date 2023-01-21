#[derive(Debug, PartialEq)]
pub enum Operator {
    Equal,
    Assign,
    //////
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    NotEqual,
    //////
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Modulo,
    //////
    BinaryAnd,
    BinaryOr,
    BinaryXor,
    BinaryNot,
    BinaryLeft,
    BinaryRight,
    //////
    LogicAnd,
    LogicOr,
    LogicNot,
}

use Operator::*;
impl Operator {
    pub fn get_precedence(&self) -> i32 {
        match self {
            BinaryNot => 250,
            LogicNot => 250,
            Power => 200,
            Multiply => 100,
            Divide => 100,
            Modulo => 100,
            Add => 80,
            Subtract => 80,
            BinaryRight => 60,
            BinaryLeft => 60,
            Less => 40,
            LessOrEqual => 40,
            Greater => 40,
            GreaterOrEqual => 40,
            Equal => 30,
            NotEqual => 30,
            BinaryAnd => 25,
            BinaryXor => 24,
            BinaryOr => 23,
            LogicAnd => 15,
            LogicOr => 10,
            Assign => 5,
        }
    }
}

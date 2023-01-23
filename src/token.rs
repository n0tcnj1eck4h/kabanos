use crate::operator::Operator;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    IntegerLiteral(i32),
    Operator(Operator),
    Keyword(Keyword),
    Atom(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    PRINT,
}

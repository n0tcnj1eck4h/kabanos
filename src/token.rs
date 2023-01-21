use crate::operator::Operator;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    IntegerLiteral(u32),
    Operator(Operator),
    Keyword(Keyword),
    Atom(char),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    PRINT,
}

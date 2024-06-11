#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    PrimitiveType(Type),
    IntegerLiteral(i128),
    FloatingPointLiteral(f64),
    StringLiteral(String),
    Operator(Operator),
    Keyword(Keyword),
    Atom(char),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Keyword {
    IF,
    ELSE,
    IMPORT,
    EXTERN,
    LOCAL,
    GLOBAL,
    FUNCTION,
    WHILE,
    STRUCT,
    TRUE,
    FALSE,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    //////
    ScopeResolution,
    RightArrow,
}

#[rustfmt::skip]
impl Operator {
    pub fn get_precedence(&self) -> i32 {
        match self {
           Operator::Multiply        => 100,
           Operator::Divide          => 100,
           Operator::Modulo          => 100,
           Operator::Add             => 80,
           Operator::Subtract        => 80,
           Operator::BinaryRight     => 60,
           Operator::BinaryLeft      => 60,
           Operator::Less            => 40,
           Operator::LessOrEqual     => 40,
           Operator::Greater         => 40,
           Operator::GreaterOrEqual  => 40,
           Operator::BinaryAnd       => 36,
           Operator::BinaryXor       => 33,
           Operator::BinaryOr        => 30,
           Operator::Equal           => 20,
           Operator::NotEqual        => 20,
           Operator::LogicAnd        => 15,
           Operator::LogicOr         => 10,
           Operator::Assign          => 5,
           Operator::BinaryNot       => -1,
           Operator::LogicNot        => -1,
           Operator::RightArrow      => -1,
           Operator::ScopeResolution => -1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
}

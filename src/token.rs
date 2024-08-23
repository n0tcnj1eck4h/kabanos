use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    IntegerLiteral(i128),
    FloatingPointLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
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
    GLOBAL,
    FUNCTION,
    WHILE,
    STRUCT,
    LET,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        return match s {
            "if" => Ok(Keyword::IF),
            "else" => Ok(Keyword::ELSE),
            "fn" => Ok(Keyword::FUNCTION),
            "let" => Ok(Keyword::LET),
            "global" => Ok(Keyword::GLOBAL),
            "extern" => Ok(Keyword::EXTERN),
            "struct" => Ok(Keyword::STRUCT),
            "import" => Ok(Keyword::IMPORT),
            "while" => Ok(Keyword::WHILE),
            _ => Err(()),
        };
    }
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

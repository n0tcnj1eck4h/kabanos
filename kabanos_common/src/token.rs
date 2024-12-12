use std::str::FromStr;

use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Identifier(String),
    IntegerLiteral(u64),
    FloatingPointLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Operator(Operator),
    Keyword(Keyword),
    Atom(char),
    None,
}

impl Default for TokenKind {
    fn default() -> Self {
        Self::None
    }
}

impl PartialEq<char> for Token {
    fn eq(&self, other: &char) -> bool {
        if let TokenKind::Atom(ch) = self.kind {
            ch == *other
        } else {
            false
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl PartialEq<TokenKind> for Token {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
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
    RETURN,
}

impl PartialEq<Keyword> for Token {
    fn eq(&self, other: &Keyword) -> bool {
        if let TokenKind::Keyword(keyword) = self.kind {
            keyword == *other
        } else {
            false
        }
    }
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
            "return" => Ok(Keyword::RETURN),
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
    Minus,
    Asterisk,
    Divide,
    Modulo,
    //////
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    LeftShift,
    RightShift,
    //////
    LogicAnd,
    LogicOr,
    Exclamation,
    //////
    ScopeResolution,
    As,
}

#[rustfmt::skip]
impl Operator {
    pub fn get_precedence(&self) -> i32 {
        match self {
           Operator::As              => 200,
           Operator::Asterisk        => 100,
           Operator::Divide          => 100,
           Operator::Modulo          => 100,
           Operator::Add             => 80,
           Operator::Minus           => 80,
           Operator::RightShift      => 60,
           Operator::LeftShift       => 60,
           Operator::Less            => 40,
           Operator::LessOrEqual     => 40,
           Operator::Greater         => 40,
           Operator::GreaterOrEqual  => 40,
           Operator::Ampersand       => 36,
           Operator::Caret           => 33,
           Operator::Pipe            => 30,
           Operator::Equal           => 20,
           Operator::NotEqual        => 20,
           Operator::LogicAnd        => 15,
           Operator::LogicOr         => 10,
           Operator::Assign          => 5,
           Operator::Tilde           => -1,
           Operator::Exclamation     => -1,
           Operator::ScopeResolution => -1,
        }
    }
}

impl PartialEq<Operator> for Token {
    fn eq(&self, other: &Operator) -> bool {
        if let TokenKind::Operator(op) = self.kind {
            op == *other
        } else {
            false
        }
    }
}

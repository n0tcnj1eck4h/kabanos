use crate::token::{Keyword, Operator, Token, Type};

pub struct Lexer<T> {
    stream: T,
    ch: Option<char>,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(mut stream: T) -> Self {
        let ch = stream.next();
        Lexer { stream, ch }
    }

    fn advance(&mut self) -> bool {
        self.ch = self.stream.next();
        self.ch.is_some()
    }

    fn check<F>(&mut self, f: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        self.ch.map_or(false, f)
    }

    fn accept(&mut self, c: char) -> bool {
        if self.ch == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while self.check(char::is_whitespace) {
            self.advance();
        }

        if self.check(|c| c == '#') {
            while self.check(|c| c != '\n') {
                self.advance();
            }

            return self.next();
        }

        if self.check(char::is_alphabetic) {
            let mut buf = String::new();
            while self.check(|c| c.is_alphanumeric() || c == '_') {
                buf.push(self.ch.unwrap());
                self.advance();
            }

            return Some(match buf.as_str() {
                "if" => Token::Keyword(Keyword::IF),
                "else" => Token::Keyword(Keyword::ELSE),
                "fn" => Token::Keyword(Keyword::FUNCTION),
                "local" => Token::Keyword(Keyword::LOCAL),
                "global" => Token::Keyword(Keyword::GLOBAL),
                "extern" => Token::Keyword(Keyword::EXTERN),
                "struct" => Token::Keyword(Keyword::STRUCT),
                "i32" => Token::PrimitiveType(Type::I32),
                "u32" => Token::PrimitiveType(Type::U32),
                "i64" => Token::PrimitiveType(Type::I64),
                "u64" => Token::PrimitiveType(Type::U64),
                "f32" => Token::PrimitiveType(Type::F32),
                "f64" => Token::PrimitiveType(Type::F64),
                _ => Token::Identifier(buf),
            });
        }

        if self.check(char::is_numeric) {
            let mut n = 0i128;
            while self.check(char::is_numeric) {
                n *= 10;
                n += self.ch.unwrap().to_digit(10).unwrap() as i128;
                self.advance();
            }

            return Some(Token::IntegerLiteral(n));
        }

        // TODO FLOATS

        if let Some(ch) = self.ch {
            self.advance();
            if let Some(op) = match ch {
                '=' => Some(if self.accept('=') {
                    Operator::Equal
                } else {
                    Operator::Assign
                }),
                '*' => Some(Operator::Multiply),
                '<' => Some(if self.accept('=') {
                    Operator::LessOrEqual
                } else if self.accept('<') {
                    Operator::BinaryLeft
                } else {
                    Operator::Less
                }),
                '>' => Some(if self.accept('=') {
                    Operator::GreaterOrEqual
                } else if self.accept('>') {
                    Operator::BinaryRight
                } else {
                    Operator::Greater
                }),
                '!' => Some(if self.accept('=') {
                    Operator::NotEqual
                } else {
                    Operator::LogicNot
                }),
                '&' => Some(if self.accept('&') {
                    Operator::LogicAnd
                } else {
                    Operator::BinaryAnd
                }),
                '|' => Some(if self.accept('|') {
                    Operator::LogicOr
                } else {
                    Operator::BinaryOr
                }),
                '+' => Some(Operator::Add),
                '-' => Some(Operator::Subtract),
                '/' => Some(Operator::Divide),
                '^' => Some(Operator::BinaryXor),
                '~' => Some(Operator::BinaryNot),
                '%' => Some(Operator::Modulo),
                _ => None,
            } {
                return Some(Token::Operator(op));
            } else {
                return Some(Token::Atom(ch));
            }
        }

        None
    }
}

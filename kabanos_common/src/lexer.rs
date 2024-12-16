use std::str::FromStr;

use crate::{
    span::{Position, Span},
    token::{Keyword, Operator, Token, TokenKind},
};

pub struct Lexer<T> {
    stream: T,
    pos: Position,
    token_start: Position,
    ch: Option<char>,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(mut stream: T) -> Self {
        let ch = stream.next();
        let pos = Position { col: 1, row: 1 };
        Lexer {
            stream,
            ch,
            pos,
            token_start: pos,
        }
    }

    fn token(&self, kind: TokenKind) -> Option<Token> {
        let span = Span {
            start: self.token_start,
            end: self.pos,
        };
        Some(Token { kind, span })
    }

    fn advance(&mut self) -> bool {
        self.ch = self.stream.next();
        if let Some('\n') = self.ch {
            self.pos.col = 1;
            self.pos.row += 1;
        } else {
            self.pos.col += 1;
        }
        self.ch.is_some()
    }

    fn check<F>(&mut self, f: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        self.ch.map_or(false, f)
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

        let ch = match self.ch {
            Some(character) => character,
            None => return None,
        };

        self.token_start = self.pos;

        // Skip comments
        if ch == '#' {
            while let Some(ch) = self.ch {
                self.advance();
                if ch == '\n' {
                    break;
                }
            }
            return self.next();
        }

        if ch.is_alphabetic() {
            let mut buf = String::new();
            while let Some(ch) = self.ch {
                if ch.is_alphanumeric() || ch == '_' {
                    buf.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }

            if let Ok(keyword) = Keyword::from_str(&buf) {
                return self.token(TokenKind::Keyword(keyword));
            }

            return match buf.as_str() {
                "false" => self.token(TokenKind::BooleanLiteral(false)),
                "true" => self.token(TokenKind::BooleanLiteral(true)),
                "as" => self.token(TokenKind::Operator(Operator::As)),
                _ => self.token(TokenKind::Identifier(buf)),
            };
        }

        if let '0'..='9' = ch {
            let mut n = 0u64;
            while let Some(ch) = self.ch {
                if let Some(d) = ch.to_digit(10) {
                    n *= 10;
                    n += d as u64;
                    self.advance();
                } else if ch == '.' {
                    self.advance();
                    let mut denominator = 1f64;
                    let mut decimals = 0f64;
                    while let Some(ch) = self.ch {
                        if let Some(d) = ch.to_digit(10) {
                            self.advance();
                            denominator *= 10.0;
                            decimals += d as f64 / denominator;
                        } else {
                            return self
                                .token(TokenKind::FloatingPointLiteral(n as f64 + decimals));
                        }
                    }
                } else {
                    break;
                }
            }
            return self.token(TokenKind::IntegerLiteral(n));
        }

        if ch == '"' {
            self.advance();
            let mut buf = String::new();
            let mut escaped = false;
            while let Some(ch) = self.ch {
                match (ch, escaped) {
                    ('"', false) => break,
                    ('\\', false) => {
                        escaped = true;
                        continue;
                    }
                    ('\\', true) => buf.push('\\'),
                    ('n', true) => buf.push('\n'),
                    ('"', true) => buf.push('"'),
                    (_, false) => buf.push(ch),
                    (_, true) => {
                        buf.push('\\');
                        buf.push(ch);
                    }
                }
                escaped = false;
                self.advance();
            }
            return self.token(TokenKind::StringLiteral(buf));
        }

        self.advance();
        let ch2 = self.ch;

        #[rustfmt::skip]
        let op = match (ch, ch2) {
            ('=', Some('=')) => { self.advance(); Some(Operator::Equal) }
            ('<', Some('=')) => { self.advance(); Some(Operator::LessOrEqual) }
            ('>', Some('=')) => { self.advance(); Some(Operator::GreaterOrEqual) }
            ('>', Some('>')) => { self.advance(); Some(Operator::RightShift) }
            ('<', Some('<')) => { self.advance(); Some(Operator::LeftShift) }
            ('!', Some('=')) => { self.advance(); Some(Operator::NotEqual) }
            ('&', Some('&')) => { self.advance(); Some(Operator::LogicAnd) }
            ('|', Some('|')) => { self.advance(); Some(Operator::LogicOr) }
            (':', Some(':')) => { self.advance(); Some(Operator::ScopeResolution) }
            ('+', _) => Some(Operator::Add),
            ('-', _) => Some(Operator::Minus),
            ('*', _) => Some(Operator::Asterisk),
            ('/', _) => Some(Operator::Divide),
            ('%', _) => Some(Operator::Modulo),
            ('&', _) => Some(Operator::Ampersand),
            ('|', _) => Some(Operator::Pipe),
            ('^', _) => Some(Operator::Caret),
            ('~', _) => Some(Operator::Tilde),
            ('=', _) => Some(Operator::Assign),
            ('<', _) => Some(Operator::Less),
            ('>', _) => Some(Operator::Greater),
            ('!', _) => Some(Operator::Exclamation),
            _ => None,
        };

        if let Some(op) = op {
            self.token(TokenKind::Operator(op))
        } else {
            self.token(TokenKind::Atom(ch))
        }
    }
}

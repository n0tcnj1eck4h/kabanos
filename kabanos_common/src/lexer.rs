use std::str::FromStr;

use crate::{
    span::{Span, Spanned, WithSpan},
    token::{Keyword, Operator, Token},
};

#[derive(Debug)]
pub enum LexerError {
    UnexpectedEOF,
    CharTooLong,
    BadEscape,
}

pub struct Lexer<T> {
    stream: T,
    span: Span,
    ch: Option<char>,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(mut stream: T) -> Self {
        let ch = stream.next();
        let span = Span::default();
        Lexer { stream, ch, span }
    }

    fn token(&mut self, kind: Token) -> Option<Result<Spanned<Token>, LexerError>> {
        let span = self.span;
        self.span.start = self.span.end;
        Some(Ok(kind.with_span(span)))
    }

    fn advance(&mut self) -> bool {
        self.ch = self.stream.next();
        if let Some('\n') = self.ch {
            self.span.end.col = 0;
            self.span.end.row += 1;
        } else {
            self.span.end.col += 1;
        }
        self.ch.is_some()
    }

    fn read_char(&mut self) -> Result<char, LexerError> {
        let mut ch = self.ch.ok_or(LexerError::UnexpectedEOF)?;

        if ch == '\\' {
            self.advance();
            ch = match self.ch.ok_or(LexerError::UnexpectedEOF)? {
                '\\' => '\\',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '"' => '"',
                _ => return Err(LexerError::BadEscape),
            }
        }

        self.advance();
        return Ok(ch);
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Result<Spanned<Token>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.ch.map_or(false, char::is_whitespace) {
            self.advance();
        }

        let ch = self.ch?;

        if ch == '#' {
            self.advance();
            while let Some(ch) = self.ch {
                self.advance();
                if ch == '\n' {
                    return self.next();
                }
            }
        }

        if ch.is_alphabetic() || ch == '_' {
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
                return self.token(Token::Keyword(keyword));
            }

            return match buf.as_str() {
                "false" => self.token(Token::BooleanLiteral(false)),
                "true" => self.token(Token::BooleanLiteral(true)),
                _ => self.token(Token::Identifier(buf)),
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
                            let f = n as f64 + decimals;
                            return self.token(Token::FloatingPointLiteral(f));
                        }
                    }
                } else {
                    break;
                }
            }
            return self.token(Token::IntegerLiteral(n));
        }

        if ch == '"' {
            self.advance();
            let mut buf = String::new();
            while self.ch != Some('"') {
                match self.read_char() {
                    Ok(ch) => buf.push(ch),
                    Err(err) => return Some(Err(err)),
                };
            }
            self.advance();
            return self.token(Token::StringLiteral(buf));
        }

        if ch == '\'' {
            self.advance();
            let ch = match self.read_char() {
                Ok(ch) => ch,
                Err(err) => return Some(Err(err)),
            };

            if self.ch != Some('\'') {
                return Some(Err(LexerError::CharTooLong));
            }

            self.advance();
            return self.token(Token::Char(ch));
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
            self.token(Token::Operator(op))
        } else {
            self.token(Token::Atom(ch))
        }
    }
}

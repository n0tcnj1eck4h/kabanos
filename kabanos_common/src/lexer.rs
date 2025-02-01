use std::{fmt::Display, mem, str::FromStr};

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

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedEOF => write!(f, "Unexpected end of file"),
            LexerError::CharTooLong => write!(f, "Too many characters in a char literal"),
            LexerError::BadEscape => write!(f, "Bad escape sequence"),
        }
    }
}

pub struct Lexer<T> {
    stream: T,
    span: Span,
    ch: Option<char>,
    buf: String,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(mut stream: T) -> Self {
        let ch = stream.next();
        let span = Span::default();
        Lexer {
            stream,
            ch,
            span,
            buf: String::new(),
        }
    }

    fn token(&mut self, kind: Token) -> Option<Result<Spanned<Token>, Spanned<LexerError>>> {
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

    fn read_char(&mut self) -> Result<char, Spanned<LexerError>> {
        let mut ch = self
            .ch
            .ok_or(LexerError::UnexpectedEOF.with_span(self.span))?;

        if ch == '\\' {
            self.advance();
            ch = match self
                .ch
                .ok_or(LexerError::UnexpectedEOF.with_span(self.span))?
            {
                '\\' => '\\',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '"' => '"',
                _ => return Err(LexerError::BadEscape.with_span(self.span)),
            }
        }

        self.advance();
        Ok(ch)
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Result<Spanned<Token>, Spanned<LexerError>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.ch.map_or(false, char::is_whitespace) {
            self.advance();
        }

        let ch = self.ch?;

        // Skip comments
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
            self.buf.clear();
            while let Some(ch) = self.ch {
                if ch.is_alphanumeric() || ch == '_' {
                    self.buf.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }

            if let Ok(keyword) = Keyword::from_str(&self.buf) {
                return self.token(Token::Keyword(keyword));
            }

            return match self.buf.as_str() {
                "false" => self.token(Token::BooleanLiteral(false)),
                "true" => self.token(Token::BooleanLiteral(true)),
                _ => {
                    let identifier = mem::take(&mut self.buf);
                    self.token(Token::Identifier(identifier))
                }
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
            self.buf.clear();
            while self.ch != Some('"') {
                match self.read_char() {
                    Ok(ch) => self.buf.push(ch),
                    Err(err) => return Some(Err(err)),
                };
            }
            self.advance();
            let literal = mem::take(&mut self.buf);
            return self.token(Token::StringLiteral(literal));
        }

        if ch == '\'' {
            self.advance();
            let ch = match self.read_char() {
                Ok(ch) => ch,
                Err(err) => return Some(Err(err)),
            };

            if self.ch != Some('\'') {
                return Some(Err(LexerError::CharTooLong.with_span(self.span)));
            }

            self.advance();
            return self.token(Token::Char(ch));
        }

        self.advance();
        use Operator::*;
        #[rustfmt::skip]
        let op = match (ch, self.ch) {
            ('=', Some('=')) => { self.advance(); Equal }
            ('<', Some('=')) => { self.advance(); LessOrEqual }
            ('>', Some('=')) => { self.advance(); GreaterOrEqual }
            ('>', Some('>')) => { self.advance(); RightShift }
            ('<', Some('<')) => { self.advance(); LeftShift }
            ('!', Some('=')) => { self.advance(); NotEqual }
            ('&', Some('&')) => { self.advance(); LogicAnd }
            ('|', Some('|')) => { self.advance(); LogicOr }
            (':', Some(':')) => { self.advance(); ScopeResolution }
            ('+', _) => Plus,
            ('-', _) => Minus,
            ('*', _) => Asterisk,
            ('/', _) => Divide,
            ('%', _) => Modulo,
            ('&', _) => Ampersand,
            ('|', _) => Pipe,
            ('^', _) => Caret,
            ('~', _) => Tilde,
            ('=', _) => Assign,
            ('<', _) => Less,
            ('>', _) => Greater,
            ('!', _) => Exclamation,
            _ => return self.token(Token::Atom(ch)),
        };
        self.token(Token::Operator(op))
    }
}

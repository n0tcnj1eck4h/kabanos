use crate::{ast::ExpressionAST, lexer::Lexer, operator::Operator, token::Token, value::Value};

#[derive(Debug)]
pub enum Error {
    UnexpectedTokenError(Option<Token>, Token),
    ExpressionExpectedError(Option<Token>),
    OperatorExpectedError(Option<Token>),
}

pub struct Parser<T: Iterator<Item = char>> {
    lexer: Lexer<T>,
    current_token: Option<Token>,
    previous_token: Option<Token>,
}

impl<T: Iterator<Item = char>> Parser<T> {
    pub fn new(mut lexer: Lexer<T>) -> Self {
        let token = lexer.get_token();
        Self {
            lexer,
            current_token: token,
            previous_token: None,
        }
    }

    fn advance(&mut self) {
        self.previous_token = self.current_token.to_owned();
        self.current_token = self.lexer.get_token();
    }

    fn expect(&mut self, token: Token) -> Result<(), Error> {
        if self.current_token == Some(token.to_owned()) {
            self.advance();
            Ok(())
        } else {
            Err(Error::UnexpectedTokenError(
                self.current_token.to_owned(),
                token,
            ))
        }
    }

    fn accept(&mut self, token: Token) -> bool {
        if self.current_token == Some(token.to_owned()) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn primary(&mut self) -> Result<Box<ExpressionAST>, Error> {
        // println!("primary");
        match self.current_token {
            Some(Token::IntegerLiteral(integer)) => self.literal(integer),
            Some(Token::Atom('(')) => self.parenthesis_expression(),
            _ => Err(Error::ExpressionExpectedError(
                self.current_token.to_owned(),
            )),
        }
    }

    fn literal(&mut self, integer: u32) -> Result<Box<ExpressionAST>, Error> {
        // println!("literal");
        self.advance();
        Ok(Box::new(ExpressionAST::Literal(Value::Integer(integer))))
    }

    pub fn expression(&mut self) -> Result<Box<ExpressionAST>, Error> {
        // println!("expression");
        let prefix = if self.accept(Token::Operator(Operator::BinaryNot))
            || self.accept(Token::Operator(Operator::Subtract))
        {
            if let Some(Token::Operator(op)) = self.previous_token {
                Some(op)
            } else {
                unreachable!()
            }
        } else {
            None
        };

        let mut lhs = self.term()?;

        if let Some(prefix) = prefix {
            lhs = Box::new(ExpressionAST::UnaryOperation(prefix, lhs))
        }

        while self.accept(Token::Operator(Operator::Add))
            || self.accept(Token::Operator(Operator::Subtract))
        {
            if let Some(Token::Operator(op)) = self.previous_token {
                let rhs = self.term()?;
                return Ok(Box::new(ExpressionAST::BinaryOperation(lhs, op, rhs)));
            } else {
                unreachable!()
            }
        }

        Ok(lhs)
    }

    pub fn term(&mut self) -> Result<Box<ExpressionAST>, Error> {
        // println!("term");
        let lhs = self.factor()?;
        while self.accept(Token::Operator(Operator::Multiply))
            || self.accept(Token::Operator(Operator::Divide))
        {
            if let Some(Token::Operator(op)) = self.previous_token {
                let rhs = self.factor()?;
                return Ok(Box::new(ExpressionAST::BinaryOperation(lhs, op, rhs)));
            } else {
                unreachable!()
            }
        }

        Ok(lhs)
    }

    pub fn factor(&mut self) -> Result<Box<ExpressionAST>, Error> {
        // println!("factor");
        let lhs = self.exponent()?;
        while self.accept(Token::Operator(Operator::Multiply))
            || self.accept(Token::Operator(Operator::Divide))
        {
            if let Some(Token::Operator(op)) = self.previous_token {
                let rhs = self.exponent()?;
                return Ok(Box::new(ExpressionAST::BinaryOperation(lhs, op, rhs)));
            } else {
                unreachable!()
            }
        }

        Ok(lhs)
    }

    pub fn exponent(&mut self) -> Result<Box<ExpressionAST>, Error> {
        // println!("exponent");
        let lhs = self.primary()?;
        while self.accept(Token::Operator(Operator::Power))
            || self.accept(Token::Operator(Operator::Modulo))
        {
            if let Some(Token::Operator(op)) = self.previous_token {
                let rhs = self.primary()?;
                return Ok(Box::new(ExpressionAST::BinaryOperation(lhs, op, rhs)));
            } else {
                unreachable!()
            }
        }

        Ok(lhs)
    }

    fn parenthesis_expression(&mut self) -> Result<Box<ExpressionAST>, Error> {
        // println!("parenthesis");
        self.expect(Token::Atom('('))?;
        let expr = self.expression();
        self.expect(Token::Atom(')'))?;

        expr
    }
}

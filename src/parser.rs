use crate::{ast::ExpressionAST, lexer::Lexer, token::Token, value::Value};

pub enum Error {
    UnexpectedTokenError(Option<Token>, Token),
    ExpressionExpectedError(Option<Token>)
}

pub struct Parser<T: Iterator<Item = char>> {
    lexer: Lexer<T>,
    current_token: Option<Token>,
}

impl<T: Iterator<Item = char>> Parser<T> {
    pub fn new(mut lexer: Lexer<T>) -> Self {
        let token = lexer.get_token();
        Self {
            lexer,
            current_token: token,
        }
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.get_token();
    }

    fn expect(&mut self, token: Token) -> Result<(), Error> {
        if self.current_token == Some(token.to_owned()) {
            self.advance();
            Ok(())
        } else {
            Err(Error::UnexpectedTokenError(self.current_token.to_owned(), token))
        }
    }

    fn primary_expression(&mut self) -> Result<Box<ExpressionAST>, Error> {
        match self.current_token {
            Some(Token::IntegerLiteral(integer)) => self.literal(integer),
            Some(Token::Atom('(')) => self.parenthesis_expression(),
            _ => Err(Error::ExpressionExpectedError(self.current_token.to_owned()))
        }
    }

    fn literal(&mut self, integer: u32) -> Result<Box<ExpressionAST>, Error> {
        self.advance();
        Ok(Box::new(ExpressionAST::Literal(Value::Integer(integer))))
    }

    pub fn expression(&mut self) -> Result<Box<ExpressionAST>, Error> {
        let lhs = self.primary_expression()?;
        self.rhs_binary_expression(0, lhs)
    }

    fn rhs_binary_expression(
        &mut self,
        precedence: i32,
        lhs: Box<ExpressionAST>,
    ) -> Result<Box<ExpressionAST>, Error> {
        // loop {
        //     if let Some(Token::Operator(op)) = self.current_token {
        //         let operator_precedence = op.get_precedence();
        //         if precedence > operator_precedence {
        //             return Some(lhs);
        //         }
        //         self.advance();
        //
        //         let rhs = self.primary_expression()?;
        //         let mut next_precedence = -1;
        //
        //         if let Some(Token::Operator(next_op)) = self.current_token {
        //             next_precedence = next_op.get_precedence();
        //         }
        //
        //         if operator_precedence < next_precedence {
        //             rhs
        //         }
        //     }
        //     else {
        //         return Some(lhs);
        //     }
        // }
        Ok(lhs)
    }

    fn parenthesis_expression(&mut self) -> Result<Box<ExpressionAST>, Error> {
        self.expect(Token::Atom('('))?;
        let expr = self.expression();
        self.expect(Token::Atom(')'))?;

        expr
    }
}

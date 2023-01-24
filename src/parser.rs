use crate::{
    ast::{expression::ExpressionAST, statement::StatementAST}, lexer::Lexer, operator::Operator, token::{Token, Keyword}, value::Value,
};

#[derive(Debug)]
pub enum Error {
    UnexpectedTokenError(Option<Token>, Token),
    ExpressionExpectedError(Option<Token>),
    StatementExpectedError(Option<Token>),
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
        if self.current_token == Some(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn statement(&mut self) -> Result<Box<StatementAST>, Error> {
        if let Some(Token::Keyword(keyword)) = &self.current_token {
            match keyword {
                Keyword::PRINT => self.print()
            }
        }
        else if self.current_token == Some(Token::Atom('{')) {
            self.block()
        }
        else {
            Err(Error::StatementExpectedError(self.current_token.to_owned()))
        }
    }

    fn print(&mut self) -> Result<Box<StatementAST>, Error> {
        self.advance();
        let expr = self.expression()?;
        self.expect(Token::Atom(';'))?;
        
        Ok(Box::new(StatementAST::Print(expr)))
    }

    fn block(&mut self) -> Result<Box<StatementAST>, Error> {
        self.advance();

        let mut statements = Vec::new();
        while self.current_token != Some(Token::Atom('}')) {
            statements.push(self.statement()?)
        }

        self.advance();
        Ok(Box::new(StatementAST::Block(statements)))
    }

    fn primary(&mut self) -> Result<Box<ExpressionAST>, Error> {
        // println!("primary");
        match self.current_token {
            Some(Token::IntegerLiteral(integer)) => self.literal(integer),
            Some(Token::Identifier(ref identifier)) => self.identifier(identifier.to_owned()),
            Some(Token::Operator(op)) => self.unary(op),
            Some(Token::Atom('(')) => self.parenthesis_expression(),
            _ => Err(Error::ExpressionExpectedError(
                self.current_token.to_owned(),
            )),
        }
    }

    fn literal(&mut self, integer: i32) -> Result<Box<ExpressionAST>, Error> {
        self.advance();
        Ok(Box::new(ExpressionAST::Literal(Value::Integer(integer))))
    }

    fn identifier(&mut self, identifier: String) -> Result<Box<ExpressionAST>, Error> {
        self.advance();
        Ok(Box::new(ExpressionAST::Identifier(identifier)))
    }

    fn unary(&mut self, operator: Operator) -> Result<Box<ExpressionAST>, Error> {
        self.advance();
        Ok(Box::new(ExpressionAST::UnaryOperation(
            operator,
            self.primary()?,
        )))
    }

    pub fn expression(&mut self) -> Result<Box<ExpressionAST>, Error> {
        let lhs = self.primary()?;
        self.expression_rhs(lhs, -1)
    }

    fn expression_rhs(
        &mut self,
        mut lhs: Box<ExpressionAST>,
        expression_precedence: i32,
    ) -> Result<Box<ExpressionAST>, Error> {
        while let Some(Token::Operator(op)) = self.current_token {
            let operator_percedence = op.get_precedence();
            if operator_percedence < expression_precedence {
                break;
            }

            self.advance();
            let mut rhs = self.primary()?;

            if let Some(Token::Operator(next_op)) = self.current_token {
                if operator_percedence < next_op.get_precedence() {
                    rhs = self.expression_rhs(rhs, operator_percedence + 1)?;
                }
            }

            lhs = Box::new(ExpressionAST::BinaryOperation(lhs, op, rhs))
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

use crate::{
    ast::{Expression, FunctionDefinition, GlobalVariableDefintion, Parameter, Program, Statement},
    token::{Keyword, Operator, Token},
};

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedTokenError(Option<Token>),
    ExpressionExpectedError(Option<Token>),
    StatementExpectedError(Option<Token>),
    DefinitionExpectedError(Option<Token>),
}

pub struct Parser<L> {
    lexer: L,
    current_token: Option<Token>,
    previous_token: Option<Token>,
}

impl<L> Parser<L>
where
    L: Iterator<Item = Token>,
{
    pub fn new(mut lexer: L) -> Self {
        let token = lexer.next();
        Self {
            lexer,
            current_token: token,
            previous_token: None,
        }
    }

    fn advance(&mut self) {
        self.previous_token = self.current_token.take();
        self.current_token = self.lexer.next();
    }

    fn expect(&mut self, token: Token) -> Result<(), ParsingError> {
        if self.current_token == Some(token) {
            self.advance();
            Ok(())
        } else {
            Err(ParsingError::UnexpectedTokenError(
                self.current_token.clone(),
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

    pub fn program(&mut self) -> Result<Program, ParsingError> {
        let mut function_definitions = Vec::new();
        let mut imports = Vec::new();
        let mut globals = Vec::new();

        while self.accept(Token::Keyword(Keyword::IMPORT)) {
            imports.push(self.import()?);
        }

        while let Some(ref token) = self.current_token {
            match token {
                Token::Keyword(Keyword::FUNCTION) => {
                    function_definitions.push(self.function_definition()?)
                }
                Token::Keyword(Keyword::GLOBAL) => globals.push(self.global_var()?),
                _ => return Err(ParsingError::UnexpectedTokenError(Some(token.clone()))),
            }
        }

        Ok(Program {
            imports,
            function_definitions,
            globals,
        })
    }

    pub fn parameter_list(&mut self) -> Result<Vec<Parameter>, ParsingError> {
        self.advance();
        let mut parameters = Vec::new();
        loop {
            match self.current_token {
                Some(Token::Atom(')')) => {
                    self.advance();
                    break;
                }
                Some(Token::PrimitiveType(type_)) => {
                    self.advance();
                    if let Some(Token::Identifier(name)) = self.current_token.clone() {
                        parameters.push(Parameter { type_, name })
                    }
                }
                _ => {
                    return Err(ParsingError::UnexpectedTokenError(
                        self.current_token.clone(),
                    ))
                }
            }

            if self.current_token != Some(Token::Atom(',')) {
                return Err(ParsingError::UnexpectedTokenError(
                    self.current_token.clone(),
                ));
            }
        }

        Ok(parameters)
    }

    pub fn function_definition(&mut self) -> Result<FunctionDefinition, ParsingError> {
        self.advance();
        if let Some(Token::Identifier(function_name)) = self.current_token.clone() {
            self.advance();
            if self.current_token == Some(Token::Atom('(')) {
                let parameters = self.parameter_list()?;
                let return_type = match self.current_token {
                    Some(Token::PrimitiveType(return_type)) => {
                        self.advance();
                        Some(return_type)
                    }
                    _ => None,
                };

                // todo uhh
                let body = self.block()?;

                return Ok(FunctionDefinition {
                    name: function_name,
                    body,
                    parameters,
                    return_type,
                });
            }
        }

        Err(ParsingError::UnexpectedTokenError(
            self.current_token.clone(),
        ))
    }

    pub fn import(&mut self) -> Result<String, ParsingError> {
        todo!()
    }

    pub fn while_loop(&mut self) -> Result<Statement, ParsingError> {
        todo!()
    }

    pub fn local_var(&mut self) -> Result<Statement, ParsingError> {
        todo!()
    }

    pub fn global_var(&mut self) -> Result<GlobalVariableDefintion, ParsingError> {
        todo!()
    }

    pub fn statement(&mut self) -> Result<Statement, ParsingError> {
        if let Some(Token::Keyword(keyword)) = self.current_token {
            match keyword {
                Keyword::IF => self.conditional(),
                Keyword::WHILE => self.while_loop(),
                Keyword::LOCAL => self.local_var(),
                _ => Err(ParsingError::StatementExpectedError(
                    self.current_token.to_owned(),
                )),
            }
        } else if self.current_token == Some(Token::Atom('{')) {
            self.block()
        } else {
            Err(ParsingError::StatementExpectedError(
                self.current_token.to_owned(),
            ))
        }
    }

    fn conditional(&mut self) -> Result<Statement, ParsingError> {
        self.advance();
        let expr = self.expression()?;
        let stmt = self.statement()?;

        let else_branch = if self.accept(Token::Keyword(Keyword::ELSE)) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Statement::Conditional(expr, Box::new(stmt), else_branch))
    }

    fn block(&mut self) -> Result<Statement, ParsingError> {
        self.advance();

        let mut statements = Vec::new();
        while self.current_token != Some(Token::Atom('}')) {
            statements.push(self.statement()?)
        }

        self.advance();
        Ok(Statement::Block(statements))
    }

    fn primary(&mut self) -> Result<Expression, ParsingError> {
        // println!("primary");
        match self.current_token {
            Some(Token::IntegerLiteral(integer)) => self.literal(integer),
            Some(Token::Identifier(ref identifier)) => self.identifier(identifier.to_owned()),
            Some(Token::Operator(op)) => self.unary(op),
            Some(Token::Atom('(')) => self.parenthesis_expression(),
            _ => Err(ParsingError::ExpressionExpectedError(
                self.current_token.to_owned(),
            )),
        }
    }

    fn literal(&mut self, integer: i128) -> Result<Expression, ParsingError> {
        self.advance();
        Ok(Expression::IntegerLiteral(integer))
    }

    fn identifier(&mut self, identifier: String) -> Result<Expression, ParsingError> {
        self.advance();
        Ok(Expression::Identifier(identifier))
    }

    fn unary(&mut self, operator: Operator) -> Result<Expression, ParsingError> {
        self.advance();
        Ok(Expression::UnaryOperation(
            operator,
            Box::new(self.primary()?),
        ))
    }

    pub fn expression(&mut self) -> Result<Expression, ParsingError> {
        let lhs = self.primary()?;
        self.expression_rhs(lhs, -1)
    }

    fn expression_rhs(
        &mut self,
        mut lhs: Expression,
        expression_precedence: i32,
    ) -> Result<Expression, ParsingError> {
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

            lhs = Expression::BinaryOperation(Box::new(lhs), op, Box::new(rhs))
        }

        Ok(lhs)
    }

    fn parenthesis_expression(&mut self) -> Result<Expression, ParsingError> {
        self.expect(Token::Atom('('))?;
        let expr = self.expression();
        self.expect(Token::Atom(')'))?;

        expr
    }
}

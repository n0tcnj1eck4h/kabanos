use std::mem;

use crate::{
    ast::{
        Composite, CompositeField, Expression, FunctionDefinition, FunctionPrototype,
        GlobalVariableDefintion, Import, Module, Parameter, Statement,
    },
    token::{Keyword, Operator, Token, TokenKind},
};

#[derive(Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedTokenError(Token),
    ExpressionExpectedError(Token),
    StatementExpectedError(Token),
    UnexpectedEOF,
}

pub struct Parser<L> {
    lexer: L,
    token: Token,
    next_token: Option<Token>,
}

// pub type Result<T> = std::result::Result<T, ParsingError>;

impl<L> Parser<L>
where
    L: Iterator<Item = Token>,
{
    pub fn new(mut lexer: L) -> Option<Self> {
        let token = lexer.next()?;
        let next_token = lexer.next();
        Some(Self {
            lexer,
            token,
            next_token,
        })
    }

    fn advance(&mut self) -> Result<(), ParsingError> {
        let next_token = self.next_token.take();
        match next_token {
            Some(token) => {
                self.token = token;
                self.next_token = self.lexer.next();
            }
            None => return Err(ParsingError::UnexpectedEOF),
        }
        Ok(())
    }

    fn expect(&mut self, token: TokenKind) -> Result<(), ParsingError> {
        if self.token.kind == token {
            self.advance()?;
            Ok(())
        } else {
            Err(ParsingError::UnexpectedTokenError(mem::take(
                &mut self.token,
            )))
        }
    }

    fn error<T>(&mut self) -> Result<T, ParsingError> {
        Err(ParsingError::UnexpectedTokenError(mem::take(
            &mut self.token,
        )))
    }

    pub fn module(&mut self) -> Result<Module, ParsingError> {
        let mut fn_defs = Vec::new();
        let mut fn_decls = Vec::new();
        let mut imports = Vec::new();
        let mut globals = Vec::new();
        let mut typedefs = Vec::new();

        while self.token.kind == TokenKind::Keyword(Keyword::IMPORT) {
            imports.push(self.import()?);
        }

        while self.next_token.is_some() {
            match self.token.kind {
                TokenKind::Keyword(Keyword::FUNCTION) => {
                    fn_defs.push(self.function_definition()?);
                }
                TokenKind::Keyword(Keyword::GLOBAL) => globals.push(self.global_var()?),
                TokenKind::Keyword(Keyword::EXTERN) => {
                    fn_decls.push(self.function_declaration()?);
                }
                TokenKind::Keyword(Keyword::STRUCT) => {
                    typedefs.push(self.structure()?);
                }
                _ => self.error()?,
            }
        }

        if self.advance() == Err(ParsingError::UnexpectedEOF) {
            Ok(Module {
                imports,
                function_declarations: fn_decls,
                function_definitions: fn_defs,
                type_definitions: typedefs,
                globals,
            })
        } else {
            self.error()?
        }
    }

    fn structure(&mut self) -> Result<Composite, ParsingError> {
        self.advance()?;
        if let TokenKind::Identifier(ref mut type_name) = self.token.kind {
            let type_name = mem::take(type_name);
            let mut fields = Vec::new();
            self.advance()?;
            self.expect(TokenKind::Atom('{'))?;
            while let TokenKind::Identifier(ref mut field_type) = self.token.kind {
                let field_type = mem::take(field_type);
                self.advance()?;
                if let TokenKind::Identifier(ref mut field_name) = self.token.kind {
                    let field_name = mem::take(field_name);
                    fields.push(CompositeField {
                        name: field_name,
                        datatype: field_type,
                    });
                    self.advance()?;
                    self.expect(TokenKind::Atom(';'))?;
                } else {
                    return self.error();
                }
            }

            if self.token == '}' {
                // Special case where it's okay for the stream to end
                let _ = self.advance();
            } else {
                return self.error();
            }

            return Ok(Composite {
                name: type_name,
                fields,
            });
        }

        self.error()
    }

    fn param_list(&mut self) -> Result<Vec<Parameter>, ParsingError> {
        self.expect(TokenKind::Atom('('))?;
        let mut parameters = Vec::new();
        loop {
            if let TokenKind::Identifier(ref mut param_type) = self.token.kind {
                let param_type = mem::take(param_type);
                self.advance()?;
                if let TokenKind::Identifier(ref mut name) = self.token.kind {
                    parameters.push(Parameter {
                        ty: param_type,
                        name: mem::take(name),
                    });
                    self.advance()?;
                }
            }

            if self.token == TokenKind::Atom(')') {
                self.advance()?;
                return Ok(parameters);
            }

            self.expect(TokenKind::Atom(','))?;
        }
    }

    pub fn import(&mut self) -> Result<Import, ParsingError> {
        self.advance()?;
        let mut path = Vec::new();
        loop {
            if let TokenKind::Identifier(ref mut path_segment) = self.token.kind {
                path.push(mem::take(path_segment));
                self.advance()?;
            } else {
                return self.error();
            }

            if self.token == ';' {
                // It's okay for the file to end after an import
                let _ = self.advance();
                return Ok(Import { path });
            }

            if self.token == Operator::ScopeResolution {
                self.advance()?;
            } else {
                return self.error();
            }
        }
    }

    pub fn while_loop(&mut self) -> Result<Statement, ParsingError> {
        self.advance()?;
        Ok(Statement::Loop(
            self.expression()?,
            Box::new(self.statement()?),
        ))
    }

    pub fn local_let(&mut self) -> Result<Statement, ParsingError> {
        self.advance()?;
        if let TokenKind::Identifier(ref mut variable_name) = self.token.kind {
            let variable_name = mem::take(variable_name);
            self.advance()?;
            let explicit_type = if self.token == ':' {
                self.advance()?;
                if let TokenKind::Identifier(ref mut identifier) = self.token.kind {
                    let explicit_type = Some(mem::take(identifier));
                    self.advance()?;
                    explicit_type
                } else {
                    return self.error();
                }
            } else {
                None
            };

            let initial_value = if self.token == Operator::Assign {
                self.advance()?;
                Some(self.expression()?)
            } else {
                None
            };

            self.expect(TokenKind::Atom(';'))?;
            return Ok(Statement::LocalVar(
                variable_name,
                explicit_type,
                initial_value,
            ));
        }

        self.error()
    }

    pub fn global_var(&mut self) -> Result<GlobalVariableDefintion, ParsingError> {
        self.advance()?;
        if let TokenKind::Identifier(ref mut datatype) = self.token.kind {
            let datatype = mem::take(datatype);
            self.advance()?;
            if let TokenKind::Identifier(ref mut name) = self.token.kind {
                let name = mem::take(name);
                self.advance()?;
                self.expect(TokenKind::Atom(';'))?;
                return Ok(GlobalVariableDefintion { datatype, name });
            }
        }

        self.error()
    }

    pub fn statement(&mut self) -> Result<Statement, ParsingError> {
        if let TokenKind::Keyword(keyword) = self.token.kind {
            match keyword {
                Keyword::IF => self.conditional(),
                Keyword::WHILE => self.while_loop(),
                Keyword::LET => self.local_let(),
                Keyword::RETURN => self.ret(),
                _ => Err(ParsingError::StatementExpectedError(mem::take(
                    &mut self.token,
                ))),
            }
        } else if self.token == '{' {
            self.block()
        } else {
            let expr_statement = Statement::Expression(self.expression()?);
            self.expect(TokenKind::Atom(';'))?;
            Ok(expr_statement)
        }
    }

    fn ret(&mut self) -> Result<Statement, ParsingError> {
        self.advance()?;
        if self.token != ';' {
            let expression = self.expression()?;
            self.expect(TokenKind::Atom(';'))?;
            Ok(Statement::Return(Some(expression)))
        } else {
            self.advance()?;
            Ok(Statement::Return(None))
        }
    }

    fn conditional(&mut self) -> Result<Statement, ParsingError> {
        self.advance()?;
        let expr = self.expression()?;
        let stmt = self.statement()?;

        let else_branch = if self.token == Keyword::ELSE {
            self.advance()?;
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Statement::Conditional(expr, Box::new(stmt), else_branch))
    }

    fn block(&mut self) -> Result<Statement, ParsingError> {
        self.advance()?;

        let mut statements = Vec::new();
        while self.token != '}' {
            statements.push(self.statement()?)
        }

        self.advance()?;
        Ok(Statement::Block(statements))
    }

    fn fn_body(&mut self) -> Result<Vec<Statement>, ParsingError> {
        self.advance()?;

        let mut statements = Vec::new();
        while self.token != '}' {
            statements.push(self.statement()?)
        }

        // it's okay for the file to end after the closing brace
        let _ = self.advance();
        Ok(statements)
    }

    fn primary(&mut self) -> Result<Expression, ParsingError> {
        match self.token.kind {
            TokenKind::IntegerLiteral(integer) => {
                self.advance()?;
                Ok(Expression::IntegerLiteral(integer))
            }
            TokenKind::FloatingPointLiteral(float) => {
                self.advance()?;
                Ok(Expression::FloatingPointLiteral(float))
            }
            TokenKind::BooleanLiteral(boolean) => {
                self.advance()?;
                Ok(Expression::BooleanLiteral(boolean))
            }
            TokenKind::Identifier(ref mut identifier) => {
                let identifier = mem::take(identifier);
                self.advance()?;
                if let TokenKind::Atom('(') = self.token.kind {
                    let mut args = Vec::new();
                    self.advance()?;
                    while self.token != ')' {
                        args.push(self.expression()?);
                        if self.token != ',' {
                            break;
                        }
                        self.advance()?;
                    }
                    self.advance()?;
                    Ok(Expression::FunctionCall(identifier, args))
                } else {
                    Ok(Expression::Identifier(identifier))
                }
            }
            TokenKind::StringLiteral(ref mut literal) => {
                let literal = mem::take(literal);
                self.advance()?;
                Ok(Expression::StringLiteral(literal))
            }
            TokenKind::Operator(op) => self.unary(op),
            TokenKind::Atom('(') => self.parenthesis_expression(),
            _ => Err(ParsingError::ExpressionExpectedError(mem::take(
                &mut self.token,
            ))),
        }
    }

    fn unary(&mut self, operator: Operator) -> Result<Expression, ParsingError> {
        self.advance()?;
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
        while let TokenKind::Operator(op) = self.token.kind {
            let operator_percedence = op.get_precedence();
            if operator_percedence < expression_precedence {
                break;
            }

            self.advance()?;
            let mut rhs = self.primary()?;

            if let TokenKind::Operator(next_op) = self.token.kind {
                if operator_percedence < next_op.get_precedence() {
                    rhs = self.expression_rhs(rhs, operator_percedence + 1)?;
                }
            }

            lhs = Expression::BinaryOperation(Box::new(lhs), op, Box::new(rhs))
        }

        Ok(lhs)
    }

    fn parenthesis_expression(&mut self) -> Result<Expression, ParsingError> {
        self.expect(TokenKind::Atom('('))?;
        let expr = self.expression();
        self.expect(TokenKind::Atom(')'))?;

        expr
    }

    fn function_definition(&mut self) -> Result<FunctionDefinition, ParsingError> {
        let prototype = self.function_prototype()?;
        if self.token == '{' {
            let body = self.fn_body()?;
            return Ok(FunctionDefinition { prototype, body });
        }

        return self.error();
    }

    fn function_declaration(&mut self) -> Result<FunctionPrototype, ParsingError> {
        self.advance()?;
        if self.token == Keyword::FUNCTION {
            self.function_prototype()
        } else {
            self.error()
        }
    }

    fn function_prototype(&mut self) -> Result<FunctionPrototype, ParsingError> {
        self.advance()?;

        let calling_convention = if let TokenKind::StringLiteral(ref mut c) = self.token.kind {
            let c = Some(mem::take(c));
            self.advance()?;
            c
        } else {
            None
        };

        if let TokenKind::Identifier(ref mut name) = self.token.kind {
            let name = mem::take(name);
            self.advance()?;
            if self.token == '(' {
                let parameters = self.param_list()?;
                let mut return_type = None;
                if self.token == ':' {
                    self.advance()?;
                    if let TokenKind::Identifier(ref mut ret_type) = self.token.kind {
                        return_type = Some(mem::take(ret_type));
                        self.advance()?;
                    }
                }
                return Ok(FunctionPrototype {
                    name,
                    parameters,
                    calling_convention,
                    return_type,
                });
            }
        }

        self.error()
    }
}

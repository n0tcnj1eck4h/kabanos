use crate::ast::error::ParsingError;
use std::mem;

use crate::{
    ast::{
        Composite, CompositeField, Expression, ExpressionKind, FunctionDefinition,
        FunctionPrototype, GlobalVariableDefintion, Import, Module, Parameter, Statement,
    },
    token::{Keyword, Operator, Token, TokenKind},
};

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
            self.error()
        }
    }

    fn error<T>(&mut self) -> Result<T, ParsingError> {
        Err(ParsingError::UnexpectedTokenError(mem::take(
            &mut self.token,
        )))
    }

    pub fn module(&mut self) -> Result<Module, ParsingError> {
        let mut fn_definitions = Vec::new();
        let mut fn_declarations = Vec::new();
        let mut imports = Vec::new();
        let mut globals = Vec::new();
        let mut ty_definitions = Vec::new();

        while self.token.kind == TokenKind::Keyword(Keyword::IMPORT) {
            imports.push(self.import()?);
        }

        while self.next_token.is_some() {
            match self.token.kind {
                TokenKind::Keyword(Keyword::FUNCTION) => {
                    fn_definitions.push(self.function_definition()?);
                }
                TokenKind::Keyword(Keyword::GLOBAL) => globals.push(self.global_var()?),
                TokenKind::Keyword(Keyword::EXTERN) => {
                    fn_declarations.push(self.function_declaration()?);
                }
                TokenKind::Keyword(Keyword::STRUCT) => {
                    ty_definitions.push(self.structure()?);
                }
                _ => self.error()?,
            }
        }

        if self.advance() == Err(ParsingError::UnexpectedEOF) {
            Ok(Module {
                imports,
                fn_declarations,
                fn_definitions,
                ty_definitions,
                globals,
            })
        } else {
            self.error()?
        }
    }

    fn structure(&mut self) -> Result<Composite, ParsingError> {
        self.advance()?;

        let TokenKind::Identifier(ref mut type_name) = self.token.kind else {
            return self.error();
        };

        let type_name = mem::take(type_name);
        let mut fields = Vec::new();

        self.advance()?;
        self.expect(TokenKind::Atom('{'))?;

        while let TokenKind::Identifier(ref mut field_type) = self.token.kind {
            let field_type = mem::take(field_type);
            self.advance()?;

            let TokenKind::Identifier(ref mut field_name) = self.token.kind else {
                return self.error();
            };

            let field_name = mem::take(field_name);
            fields.push(CompositeField {
                name: field_name,
                datatype: field_type,
            });

            self.advance()?;
            self.expect(TokenKind::Atom(';'))?;
        }

        if self.token == '}' {
            // Special case where it's okay for the stream to end
            let _ = self.advance();
        } else {
            return self.error();
        }

        Ok(Composite {
            name: type_name,
            fields,
        })
    }

    fn param_list(&mut self) -> Result<Vec<Parameter>, ParsingError> {
        self.expect(TokenKind::Atom('('))?;
        let mut parameters = Vec::new();
        loop {
            let mut span = self.token.span;
            if let TokenKind::Identifier(ref mut param_type) = self.token.kind {
                let param_type = mem::take(param_type);
                self.advance()?;
                if let TokenKind::Identifier(ref mut name) = self.token.kind {
                    span = span.join(self.token.span);
                    parameters.push(Parameter {
                        ty: param_type,
                        name: mem::take(name),
                        span,
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
            let TokenKind::Identifier(ref mut path_segment) = self.token.kind else {
                return self.error();
            };

            path.push(mem::take(path_segment));
            self.advance()?;

            if self.token == ';' {
                // It's okay for the file to end after an import
                let _ = self.advance();
                return Ok(Import { path });
            }

            self.expect(TokenKind::Operator(Operator::ScopeResolution))?;
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

        let TokenKind::Identifier(ref mut variable_name) = self.token.kind else {
            return self.error();
        };

        let variable_name = mem::take(variable_name);
        self.advance()?;

        let mut explicit_type = None;
        if self.token == ':' {
            self.advance()?;

            let TokenKind::Identifier(ref mut identifier) = self.token.kind else {
                return self.error();
            };

            explicit_type = Some(mem::take(identifier));
            self.advance()?;
        }

        let initial_value = if self.token == Operator::Assign {
            self.advance()?;
            Some(self.expression()?)
        } else {
            None
        };

        self.expect(TokenKind::Atom(';'))?;
        Ok(Statement::LocalVar(
            variable_name,
            explicit_type,
            initial_value,
        ))
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
        let span = self.token.span;
        match self.token.kind {
            TokenKind::IntegerLiteral(integer) => {
                self.advance()?;
                let kind = ExpressionKind::IntegerLiteral(integer);
                Ok(Expression { kind, span })
            }
            TokenKind::FloatingPointLiteral(float) => {
                self.advance()?;
                let kind = ExpressionKind::FloatLiteral(float);
                Ok(Expression { kind, span })
            }
            TokenKind::BooleanLiteral(boolean) => {
                self.advance()?;
                let kind = ExpressionKind::BooleanLiteral(boolean);
                Ok(Expression { kind, span })
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
                    let span = span.join(self.token.span);
                    self.advance()?;
                    let kind = ExpressionKind::FunctionCall(identifier, args);
                    Ok(Expression { kind, span })
                } else {
                    let kind = ExpressionKind::Identifier(identifier);
                    Ok(Expression { kind, span })
                }
            }
            TokenKind::StringLiteral(ref mut literal) => {
                let literal = mem::take(literal);
                self.advance()?;
                let kind = ExpressionKind::StringLiteral(literal);
                Ok(Expression { kind, span })
            }
            TokenKind::Operator(op) => self.unary(op),
            TokenKind::Atom('(') => self.parenthesis_expression(),
            _ => Err(ParsingError::ExpressionExpectedError(mem::take(
                &mut self.token,
            ))),
        }
    }

    fn unary(&mut self, operator: Operator) -> Result<Expression, ParsingError> {
        let span = self.token.span;
        self.advance()?;
        let kind = ExpressionKind::UnaryOperation(operator, Box::new(self.primary()?));
        let span = span.join(self.token.span);

        Ok(Expression { kind, span })
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

            let span = lhs.span.join(rhs.span);
            let kind = ExpressionKind::BinaryOp(Box::new(lhs), op, Box::new(rhs));
            lhs = Expression { kind, span };
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

        self.error()
    }

    fn function_declaration(&mut self) -> Result<FunctionPrototype, ParsingError> {
        self.advance()?;
        if self.token == Keyword::FUNCTION {
            let prototype = self.function_prototype()?;
            self.expect(TokenKind::Atom(';'))?;
            Ok(prototype)
        } else {
            self.error()
        }
    }

    fn function_prototype(&mut self) -> Result<FunctionPrototype, ParsingError> {
        let mut span = self.token.span;
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
                span = self.token.span.join(span);
                return Ok(FunctionPrototype {
                    name,
                    parameters,
                    calling_convention,
                    return_type,
                    span,
                });
            }
        }

        self.error()
    }
}

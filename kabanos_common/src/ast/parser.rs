use crate::{
    ast::error::ParsingError,
    span::{HasSpan, Span, Spanned, WithSpan},
};
use std::mem;

use crate::{
    ast::{
        Composite, CompositeField, Expression, FunctionDefinition, FunctionPrototype,
        GlobalVariableDefintion, Import, Module, Parameter, Statement,
    },
    token::{Keyword, Operator, Token},
};

pub struct Parser<L> {
    lexer: L,
    token: Spanned<Token>,
    next_token: Option<Spanned<Token>>,
}

impl<L> Parser<L>
where
    L: Iterator<Item = Spanned<Token>>,
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
            None => {
                let pos = self.token.get_span().end;
                let span = Span {
                    start: pos,
                    end: pos,
                };

                return Err(ParsingError::UnexpectedEOF(span));
            }
        }
        Ok(())
    }

    fn expect(&mut self, token: Token) -> Result<(), ParsingError> {
        if *self.token == token {
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

        while *self.token == Token::Keyword(Keyword::IMPORT) {
            imports.push(self.import()?);
        }

        while self.next_token.is_some() {
            match *self.token {
                Token::Keyword(Keyword::FUNCTION) => {
                    fn_definitions.push(self.function_definition()?);
                }
                Token::Keyword(Keyword::GLOBAL) => globals.push(self.global_var()?),
                Token::Keyword(Keyword::EXTERN) => {
                    fn_declarations.push(self.function_declaration()?);
                }
                Token::Keyword(Keyword::STRUCT) => {
                    ty_definitions.push(self.structure()?);
                }
                _ => self.error()?,
            }
        }

        if let Err(ParsingError::UnexpectedEOF(_)) = self.advance() {
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

        let Token::Identifier(ref mut type_name) = *self.token else {
            return self.error();
        };

        let type_name = mem::take(type_name);
        let mut fields = Vec::new();

        self.advance()?;
        self.expect(Token::Atom('{'))?;

        while let Token::Identifier(ref mut field_type) = *self.token {
            let field_type = mem::take(field_type);
            self.advance()?;

            let Token::Identifier(ref mut field_name) = *self.token else {
                return self.error();
            };

            let field_name = mem::take(field_name);
            fields.push(CompositeField {
                name: field_name,
                datatype: field_type,
            });

            self.advance()?;
            self.expect(Token::Atom(';'))?;
        }

        if *self.token == '}' {
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
        self.expect(Token::Atom('('))?;
        let mut parameters = Vec::new();
        loop {
            let mut span = self.token.get_span();
            if let Token::Identifier(ref mut param_type) = *self.token {
                let param_type = mem::take(param_type);
                self.advance()?;
                span = span.join(self.token.get_span());
                if let Token::Identifier(ref mut name) = *self.token {
                    parameters.push(Parameter {
                        ty: param_type,
                        name: mem::take(name),
                        span,
                    });
                    self.advance()?;
                }
            }

            if *self.token == Token::Atom(')') {
                self.advance()?;
                return Ok(parameters);
            }

            self.expect(Token::Atom(','))?;
        }
    }

    pub fn import(&mut self) -> Result<Import, ParsingError> {
        self.advance()?;
        let mut path = Vec::new();
        loop {
            let Token::Identifier(ref mut path_segment) = *self.token else {
                return self.error();
            };

            path.push(mem::take(path_segment));
            self.advance()?;

            if *self.token == ';' {
                // It's okay for the file to end after an import
                let _ = self.advance();
                return Ok(Import { path });
            }

            self.expect(Token::Operator(Operator::ScopeResolution))?;
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

        let span = self.token.get_span();
        let Token::Identifier(ref mut variable_name) = *self.token else {
            return self.error();
        };

        let variable_name = mem::take(variable_name).with_span(span);
        self.advance()?;

        let mut explicit_type = None;
        if *self.token == ':' {
            self.advance()?;

            let span = self.token.get_span();
            let Token::Identifier(ref mut identifier) = *self.token else {
                return self.error();
            };

            explicit_type = Some(mem::take(identifier).with_span(span));
            self.advance()?;
        }

        let initial_value = if *self.token == Operator::Assign {
            self.advance()?;
            Some(self.expression()?)
        } else {
            None
        };

        self.expect(Token::Atom(';'))?;
        Ok(Statement::LocalVar(
            variable_name,
            explicit_type,
            initial_value,
        ))
    }

    pub fn global_var(&mut self) -> Result<GlobalVariableDefintion, ParsingError> {
        self.advance()?;
        if let Token::Identifier(ref mut datatype) = *self.token {
            let datatype = mem::take(datatype);
            self.advance()?;
            if let Token::Identifier(ref mut name) = *self.token {
                let name = mem::take(name);
                self.advance()?;
                self.expect(Token::Atom(';'))?;
                return Ok(GlobalVariableDefintion { datatype, name });
            }
        }

        self.error()
    }

    pub fn statement(&mut self) -> Result<Statement, ParsingError> {
        if let Token::Keyword(keyword) = *self.token {
            match keyword {
                Keyword::IF => self.conditional(),
                Keyword::WHILE => self.while_loop(),
                Keyword::LET => self.local_let(),
                Keyword::RETURN => self.ret(),
                _ => Err(ParsingError::StatementExpectedError(mem::take(
                    &mut self.token,
                ))),
            }
        } else if *self.token == '{' {
            self.block()
        } else {
            let expr_statement = Statement::Expression(self.expression()?);
            self.expect(Token::Atom(';'))?;
            Ok(expr_statement)
        }
    }

    fn ret(&mut self) -> Result<Statement, ParsingError> {
        self.advance()?;
        if *self.token != ';' {
            let expression = self.expression()?;
            self.expect(Token::Atom(';'))?;
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

        let else_branch = if *self.token == Keyword::ELSE {
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
        while *self.token != '}' {
            statements.push(self.statement()?)
        }

        self.advance()?;
        Ok(Statement::Block(statements))
    }

    fn fn_body(&mut self) -> Result<Vec<Statement>, ParsingError> {
        self.advance()?;

        let mut statements = Vec::new();
        while *self.token != '}' {
            statements.push(self.statement()?)
        }

        // it's okay for the file to end after the closing brace
        let _ = self.advance();
        Ok(statements)
    }

    fn primary(&mut self) -> Result<Spanned<Expression>, ParsingError> {
        let span = self.token.get_span();
        match *self.token {
            Token::Char(ch) => {
                self.advance()?;
                let kind = Expression::IntegerLiteral(ch.into());
                Ok(kind.with_span(span))
            }
            Token::IntegerLiteral(integer) => {
                self.advance()?;
                let kind = Expression::IntegerLiteral(integer);
                Ok(kind.with_span(span))
            }
            Token::FloatingPointLiteral(float) => {
                self.advance()?;
                let kind = Expression::FloatLiteral(float);
                Ok(kind.with_span(span))
            }
            Token::BooleanLiteral(boolean) => {
                self.advance()?;
                let kind = Expression::BooleanLiteral(boolean);
                Ok(kind.with_span(span))
            }
            Token::Identifier(ref mut identifier) => {
                let identifier = mem::take(identifier);
                self.advance()?;
                if let Token::Atom('(') = *self.token {
                    let mut args = Vec::new();
                    self.advance()?;
                    while *self.token != ')' {
                        args.push(self.expression()?);
                        if *self.token != ',' {
                            break;
                        }
                        self.advance()?;
                    }
                    let span = span.join(self.token.get_span());
                    self.advance()?;
                    let kind = Expression::FunctionCall(identifier, args);
                    Ok(kind.with_span(span))
                } else {
                    let kind = Expression::Identifier(identifier);
                    Ok(kind.with_span(span))
                }
            }
            Token::StringLiteral(ref mut literal) => {
                let literal = mem::take(literal);
                self.advance()?;
                let kind = Expression::StringLiteral(literal);
                Ok(kind.with_span(span))
            }
            Token::Operator(op) => self.unary(op),
            Token::Atom('(') => self.parenthesis_expression(),
            _ => Err(ParsingError::ExpressionExpectedError(mem::take(
                &mut self.token,
            ))),
        }
    }

    fn unary(&mut self, operator: Operator) -> Result<Spanned<Expression>, ParsingError> {
        let span = self.token.get_span();
        self.advance()?;
        let kind = Expression::UnaryOperation(operator, Box::new(self.primary()?));
        let span = span.join(self.token.get_span());

        Ok(kind.with_span(span))
    }

    pub fn expression(&mut self) -> Result<Spanned<Expression>, ParsingError> {
        let lhs = self.primary()?;
        self.expression_rhs(lhs, -1)
    }

    fn expression_rhs(
        &mut self,
        mut lhs: Spanned<Expression>,
        expression_precedence: i32,
    ) -> Result<Spanned<Expression>, ParsingError> {
        while let Token::Operator(op) = *self.token {
            let operator_percedence = op.get_precedence();
            if operator_percedence < expression_precedence {
                break;
            }

            self.advance()?;
            let mut rhs = self.primary()?;

            if let Token::Operator(next_op) = *self.token {
                if operator_percedence < next_op.get_precedence() {
                    rhs = self.expression_rhs(rhs, operator_percedence + 1)?;
                }
            }

            let span = lhs.get_span().join(rhs.get_span());
            let kind = Expression::BinaryOp(Box::new(lhs), op, Box::new(rhs));
            lhs = kind.with_span(span);
        }

        Ok(lhs)
    }

    fn parenthesis_expression(&mut self) -> Result<Spanned<Expression>, ParsingError> {
        self.expect(Token::Atom('('))?;
        let expr = self.expression();
        self.expect(Token::Atom(')'))?;

        expr
    }

    fn function_definition(&mut self) -> Result<FunctionDefinition, ParsingError> {
        let prototype = self.function_prototype()?;
        if *self.token == '{' {
            let body = self.fn_body()?;
            return Ok(FunctionDefinition { prototype, body });
        }

        self.error()
    }

    fn function_declaration(&mut self) -> Result<FunctionPrototype, ParsingError> {
        self.advance()?;
        if *self.token == Keyword::FUNCTION {
            let prototype = self.function_prototype()?;
            self.expect(Token::Atom(';'))?;
            Ok(prototype)
        } else {
            self.error()
        }
    }

    fn function_prototype(&mut self) -> Result<FunctionPrototype, ParsingError> {
        let mut span = self.token.get_span();
        self.advance()?;

        let calling_convention = if let Token::StringLiteral(ref mut c) = *self.token {
            let c = Some(mem::take(c));
            self.advance()?;
            c
        } else {
            None
        };

        if let Token::Identifier(ref mut name) = *self.token {
            let name = mem::take(name);
            self.advance()?;
            if *self.token == '(' {
                let parameters = self.param_list()?;
                let mut return_type = None;
                if *self.token == ':' {
                    self.advance()?;
                    if let Token::Identifier(ref mut ret_type) = *self.token {
                        return_type = Some(mem::take(ret_type));
                        self.advance()?;
                    }
                }
                span = self.token.get_span().join(span);
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

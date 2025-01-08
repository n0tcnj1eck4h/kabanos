use crate::{
    ast,
    span::{HasSpan, Span, Spanned, WithSpan},
};

use super::{
    error::SemanticError,
    expression::{Expression, LValue},
    symbol::{SymbolTable, Variable, VariableID},
    types::Type,
    FunctionCall, Scope, Statement,
};

pub struct Analyzer<'a, 'b> {
    pub symbol_table: &'a mut SymbolTable,
    pub expected_return_ty: Option<&'b Type>,
    pub stack: &'a mut Vec<VariableID>,
}

impl Analyzer<'_, '_> {
    pub fn build_statements<I>(&mut self, iter: I) -> Result<Vec<Statement>, Spanned<SemanticError>>
    where
        I: IntoIterator<Item = ast::Statement>,
    {
        let mut iter = iter.into_iter();
        let mut statements = Vec::new();

        while let Some(statement) = iter.next() {
            match statement {
                ast::Statement::Conditional(expr, true_block, else_block) => {
                    let expr = self.build_expression(expr, Some(&Type::Bool))?;

                    let true_block = self.build_statements(*true_block)?;

                    if let Some(else_block) = else_block {
                        let else_block = self.build_statements(*else_block)?;
                        statements.push(Statement::Conditional(expr, true_block, Some(else_block)));
                    } else {
                        statements.push(Statement::Conditional(expr, true_block, None));
                    }
                }
                ast::Statement::Expression(expr) => {
                    // Treat void funtion calls as statements
                    let span = expr.get_span();
                    let expr = expr.unwrap();
                    if let ast::Expression::FunctionCall(name, args) = &expr {
                        let fn_id =
                            self.symbol_table
                                .get_function_id_by_name(name)
                                .ok_or_else(|| {
                                    SemanticError::Undeclared(name.clone()).with_span(span)
                                })?;

                        let fn_decl = self.symbol_table.get_function(fn_id);
                        if fn_decl.ty.is_none() {
                            let params = &fn_decl.params;
                            if params.len() != args.len() {
                                return Err(SemanticError::WrongArgumentCount.with_span(span));
                            }

                            let args: Result<Vec<_>, _> = params
                                .into_iter()
                                .zip(args.into_iter())
                                .map(|(param, expr)| {
                                    self.build_expression(expr.clone(), Some(&param.ty))
                                })
                                .collect();

                            let call = FunctionCall {
                                id: fn_id,
                                args: args?,
                            };

                            statements.push(Statement::VoidFunctionCall(call));
                            continue;
                        }
                    }

                    statements.push(Statement::Expression(
                        self.build_expression(expr.with_span(span), None)?,
                    ));
                }
                ast::Statement::Loop(expr, s) => {
                    let expr = self.build_expression(expr, Some(&Type::Bool))?;
                    let s = self.build_statements(*s)?;
                    statements.push(Statement::Loop(expr, s));
                }
                ast::Statement::Block(s) => {
                    let old_len = self.stack.len();
                    let s = self.build_statements(s)?;
                    self.stack.truncate(old_len);
                    statements.extend(s);
                }
                ast::Statement::Return(expr) => match (expr, self.expected_return_ty) {
                    (Some(expr), Some(expected_return_ty)) => {
                        let expr = self.build_expression(expr, Some(expected_return_ty))?;
                        statements.push(Statement::Return(Some(expr)));
                    }
                    (None, None) => {
                        statements.push(Statement::Return(None));
                    }
                    _ => {
                        return Err(SemanticError::ReturnTypeMismatch {
                            expected: self.expected_return_ty.cloned(),
                        }
                        .with_span(Span::default()));
                    }
                },
                ast::Statement::LocalVar(identifier, ty, expr) => {
                    let ast_ty =
                        ty.ok_or(SemanticError::ImplicitType.with_span(identifier.get_span()))?;

                    let ty: Type = ast_ty.try_into()?;

                    let identifier = identifier.unwrap();
                    let symbol = Variable {
                        identifier,
                        ty: ty.clone(),
                    };
                    let symbol_id = self.symbol_table.add_variable(symbol);

                    let mut body = Vec::new();
                    if let Some(expr) = expr {
                        let expr = self.build_expression(expr, Some(&ty))?;
                        let kind =
                            Expression::Assignment(LValue::LocalVar(symbol_id), Box::new(expr));
                        body.push(Statement::Expression(kind));
                    };
                    self.stack.push(symbol_id);
                    body.extend(self.build_statements(iter)?.into_iter());
                    self.stack.pop();

                    let scope = Scope {
                        variable_id: symbol_id,
                        body,
                    };

                    statements.push(Statement::Block(scope));
                    break;
                }
            }
        }

        Ok(statements)
    }
}

pub enum StatementIter {
    Single(Option<ast::Statement>),
    Block(std::vec::IntoIter<ast::Statement>),
}

impl Iterator for StatementIter {
    type Item = ast::Statement;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            StatementIter::Single(statement) => statement.take(),
            StatementIter::Block(iter) => iter.next(),
        }
    }
}

impl IntoIterator for ast::Statement {
    type Item = ast::Statement;
    type IntoIter = StatementIter;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Block(block) => StatementIter::Block(block.into_iter()),
            _ => StatementIter::Single(Some(self)),
        }
    }
}

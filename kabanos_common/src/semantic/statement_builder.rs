use std::str::FromStr;

use crate::{ast, span::Span};

use super::{
    error::{SemanticError, SemanticErrorKind},
    expression::{Expression, ExpressionKind, LValue},
    expression_builder::ExpressionBuilder,
    primitive::Primitive,
    symbol::{SymbolTable, Variable, VariableID},
    types::TypeKind,
    Scope, Statement,
};

pub struct StatementBuilder<'a> {
    pub symbol_table: &'a mut SymbolTable,
    pub expected_return_ty: Option<TypeKind>,
    pub stack: &'a mut Vec<VariableID>,
}

impl StatementBuilder<'_> {
    pub fn build_statements<I>(&mut self, iter: I) -> Result<Vec<Statement>, SemanticError>
    where
        I: IntoIterator<Item = ast::Statement>,
    {
        let mut iter = iter.into_iter();
        let mut statements = Vec::new();

        while let Some(statement) = iter.next() {
            match statement {
                ast::Statement::Conditional(expr, true_block, else_block) => {
                    let expr = self.build_expression(expr, Some(TypeKind::Boolean))?;

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
                    if let ast::ExpressionKind::FunctionCall(ref name, ref args) = expr.kind {
                        let span = expr.span;
                        let fn_id =
                            self.symbol_table
                                .get_function_id_by_name(name)
                                .ok_or_else(|| {
                                    SemanticErrorKind::Undeclared(name.clone()).with_span(span)
                                })?;
                        let fn_decl = self.symbol_table.get_function(fn_id);
                        if fn_decl.ty.is_none() {
                            let params = &fn_decl.params;
                            if params.len() != args.len() {
                                return Err(SemanticErrorKind::WrongArgumentCount.with_span(span));
                            }
                            let args: Result<Vec<_>, _> = params
                                .iter()
                                .zip(args)
                                .map(|(param, arg)| {
                                    self.build_expression(arg.clone(), Some(param.ty))
                                })
                                .collect();

                            statements.push(Statement::VoidFunctionCall(fn_decl.clone(), args?));
                            continue;
                        }
                    }
                    statements.push(Statement::Expression(self.build_expression(expr, None)?));
                }
                ast::Statement::Loop(expr, s) => {
                    let expr = self.build_expression(expr, Some(TypeKind::Boolean))?;
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
                        return Err(SemanticErrorKind::ReturnTypeMismatch {
                            expected: self.expected_return_ty,
                        }
                        .with_span(Span::default()));
                    }
                },
                ast::Statement::LocalVar(identifier, ty, expr) => {
                    let ty_str = ty.ok_or_else(|| {
                        SemanticErrorKind::ImplicitType.with_span(Span::default())
                    })?;
                    let ty: TypeKind = Primitive::from_str(&ty_str)
                        .map_err(|e| e.with_span(Span::default()))?
                        .into();

                    let symbol = Variable { identifier, ty };
                    let symbol_id = self.symbol_table.push_local_var(symbol);

                    if let Some(expr) = expr {
                        let span = expr.span;
                        let expr = self.build_expression(expr, Some(ty))?;
                        let ty = expr.ty;
                        let kind =
                            ExpressionKind::Assignment(LValue::LocalVar(symbol_id), Box::new(expr));
                        statements.push(Statement::Expression(Expression { kind, ty, span }));
                    };

                    self.stack.push(symbol_id);
                    let s = self.build_statements(iter)?;
                    self.stack.pop();

                    let scope = Scope {
                        symbol: symbol_id,
                        body: s,
                    };

                    statements.push(Statement::Block(scope));
                    break;
                }
            }
        }

        Ok(statements)
    }

    fn build_expression(
        &self,
        expr: ast::Expression,
        expected_ty: Option<TypeKind>,
    ) -> Result<Expression, SemanticError> {
        let experssion_builder = ExpressionBuilder {
            stack: &self.stack,
            symbol_table: &self.symbol_table,
            expected_ty,
        };

        experssion_builder.build_expression(expr)
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

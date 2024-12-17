use std::str::FromStr;

use crate::{ast, span::Span};

use super::{
    error::{SemanticError, SemanticErrorKind},
    expression::{Expression, ExpressionKind, LValue},
    expression_builder::ExpressionBuilder,
    primitive::Primitive,
    symbol::{SymbolTable, Variable, VariableID},
    types::TypeKind,
    FunctionDeclaration, Parameter, Scope, Statement,
};

#[derive(Default, Debug)]
pub struct Module {
    symbol_table: SymbolTable,
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

impl Module {
    pub fn build_module(module: ast::Module) -> Result<Module, SemanticError> {
        let mut context = Module::default();

        for s in module.fn_declarations {
            let span = s.span;
            let fn_decl = context.build_declaration(s)?;
            context
                .symbol_table
                .declare_function(fn_decl)
                .map_err(|e| e.with_span(span))?;
        }

        for s in module.fn_definitions.iter() {
            let fn_decl = context.build_declaration(s.prototype.clone())?;
            context
                .symbol_table
                .declare_function(fn_decl)
                .map_err(|e| e.with_span(s.prototype.span))?;
        }

        for s in module.fn_definitions {
            let span = s.prototype.span;
            let declaration = context.build_declaration(s.prototype)?;

            let mut stack = Vec::new();
            for param in &declaration.params {
                let id = context.symbol_table.push_local_var(Variable {
                    identifier: param.name.clone(),
                    ty: param.ty,
                });
                stack.push(id);
            }
            let body = context.build_statements(s.body, &mut stack, declaration.ty)?;
            let decl_id = context
                .symbol_table
                .get_function_id_by_decl(&declaration)
                .map_err(|e| e.with_span(span))?
                .expect("Function not forward declared");
            context
                .symbol_table
                .define_function(decl_id, body)
                .map_err(|e| e.with_span(span))?;
        }

        Ok(context)
    }

    fn build_statements<I: IntoIterator<Item = ast::Statement>>(
        &mut self,
        iter: I,
        stack: &mut Vec<VariableID>,
        expected_return_ty: Option<TypeKind>,
    ) -> Result<Vec<Statement>, SemanticError> {
        let mut iter = iter.into_iter();
        let mut statements = Vec::new();

        while let Some(statement) = iter.next() {
            match statement {
                ast::Statement::Conditional(expr, true_block, else_block) => {
                    let expr = self.build_expression(expr, Some(TypeKind::Boolean), stack)?;

                    let true_block =
                        self.build_statements(*true_block, stack, expected_return_ty)?;

                    if let Some(else_block) = else_block {
                        let else_block =
                            self.build_statements(*else_block, stack, expected_return_ty)?;
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
                                    self.build_expression(arg.clone(), Some(param.ty), stack)
                                })
                                .collect();

                            statements.push(Statement::VoidFunctionCall(fn_decl.clone(), args?));
                            continue;
                        }
                    }
                    statements.push(Statement::Expression(
                        self.build_expression(expr, None, stack)?,
                    ));
                }
                ast::Statement::Loop(expr, s) => {
                    let expr = self.build_expression(expr, Some(TypeKind::Boolean), stack)?;
                    let s = self.build_statements(*s, stack, expected_return_ty)?;
                    statements.push(Statement::Loop(expr, s));
                }
                ast::Statement::Block(s) => {
                    let old_len = stack.len();
                    let s = self.build_statements(s, stack, expected_return_ty)?;
                    stack.truncate(old_len);
                    statements.extend(s);
                }
                ast::Statement::Return(expr) => match (expr, expected_return_ty) {
                    (Some(expr), Some(expected_return_ty)) => {
                        let expr = self.build_expression(expr, Some(expected_return_ty), stack)?;
                        statements.push(Statement::Return(Some(expr)));
                    }
                    (None, None) => {
                        statements.push(Statement::Return(None));
                    }
                    _ => {
                        return Err(SemanticErrorKind::ReturnTypeMismatch {
                            expected: expected_return_ty,
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
                        let expr = self.build_expression(expr, Some(ty), stack)?;
                        let ty = expr.ty;
                        let kind =
                            ExpressionKind::Assignment(LValue::LocalVar(symbol_id), Box::new(expr));
                        statements.push(Statement::Expression(Expression { kind, ty, span }));
                    };

                    stack.push(symbol_id);
                    let s = self.build_statements(iter, stack, expected_return_ty)?;
                    stack.pop();

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
        stack: &[VariableID],
    ) -> Result<Expression, SemanticError> {
        let experssion_builder = ExpressionBuilder {
            stack,
            symbol_table: &self.symbol_table,
            expected_ty,
        };

        experssion_builder.build_expression(expr)
    }

    fn build_declaration(
        &mut self,
        prototype: ast::FunctionPrototype,
    ) -> Result<FunctionDeclaration, SemanticError> {
        let name = prototype.name;
        let ty = prototype
            .return_type
            .map(|ty| Primitive::from_str(&ty))
            .transpose()
            .map_err(|e| e.with_span(prototype.span))?
            .map(Into::into);

        let mut params = Vec::new();
        for p in prototype.parameters {
            let name = p.name;
            let ty = Primitive::from_str(&p.ty)
                .map_err(|e| e.with_span(p.span))?
                .into();
            let param = Parameter { name, ty };
            params.push(param);
        }

        Ok(FunctionDeclaration { name, ty, params })
    }
}

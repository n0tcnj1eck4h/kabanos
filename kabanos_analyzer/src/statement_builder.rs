use kabanos_common::{
    ast,
    span::{HasSpan, Span, Spanned, WithSpan},
    token::Operator,
};

use super::{
    FunctionCall, Scope, Statement,
    error::SemanticError,
    expression::{Expression, LValue},
    symbol::{SymbolTable, Variable, VariableID},
    types::{Type, TypeKind},
};

pub struct Analyzer<'a> {
    pub symbol_table: &'a mut SymbolTable,
    pub expected_return_ty: Option<Type>,
    pub stack: &'a mut Vec<VariableID>,
}

pub enum ControlFlow {
    Return,
    Fallthrough,
}

impl Analyzer<'_> {
    pub fn build_statements<I>(
        &mut self,
        stmt_iter: I,
    ) -> Result<(Vec<Statement>, ControlFlow), Spanned<SemanticError>>
    where
        I: IntoIterator<Item = ast::Statement>,
    {
        let mut iter = stmt_iter.into_iter();
        let mut statements = Vec::new();
        let mut flow = ControlFlow::Fallthrough;

        while let Some(statement) = iter.next() {
            match statement {
                ast::Statement::Conditional(expr, true_block, else_block) => {
                    let expr = self.build_expression(expr, Some(TypeKind::Bool.into()))?;

                    let (true_block, true_flow) = self.build_statements([*true_block])?;
                    flow = ControlFlow::Fallthrough;

                    if let Some(else_block) = else_block {
                        let (else_block, else_flow) = self.build_statements([*else_block])?;
                        if let (ControlFlow::Return, ControlFlow::Return) = (else_flow, true_flow) {
                            flow = ControlFlow::Return;
                        }
                        statements.push(Statement::Conditional(expr, true_block, Some(else_block)));
                    } else {
                        statements.push(Statement::Conditional(expr, true_block, None));
                    }
                }
                ast::Statement::Expression(expr) => {
                    // Treat void function calls as statements
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

                            let args = params
                                .iter()
                                .zip(args.iter())
                                .map(|(param, expr)| {
                                    self.build_expression(expr.clone(), Some(param.ty))
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            let call = FunctionCall { id: fn_id, args };

                            statements.push(Statement::VoidFunctionCall(call));
                            continue;
                        }
                    }

                    if let ast::Expression::BinaryOp(left, Operator::Assign, right) = expr {
                        let left_span = left.get_span();
                        let left = self.build_expression(*left, None)?;
                        let Expression::LValue(LValue::LocalVar(variable_id)) = left else {
                            return Err(SemanticError::LValue(left).with_span(left_span));
                        };

                        let var = self.symbol_table.get_variable(variable_id);
                        let right = self.build_expression(*right, Some(var.ty))?;

                        statements.push(Statement::Assignment(
                            LValue::LocalVar(variable_id),
                            Box::new(right),
                        ));
                        continue;
                    } else {
                        statements.push(Statement::Expression(
                            self.build_expression(expr.with_span(span), None)?,
                        ));
                        flow = ControlFlow::Fallthrough;
                    }
                }
                ast::Statement::Loop(expr, s) => {
                    let expr = self.build_expression(expr, Some(TypeKind::Bool.into()))?;
                    let (s, clfow) = self.build_statements([*s])?;
                    flow = clfow;
                    if let ControlFlow::Return = flow {
                        println!("Why would you make a loop that runs once");
                    }
                    statements.push(Statement::Loop(expr, s));
                }
                ast::Statement::Block(s) => {
                    let (s, cflow) = self.build_statements(s)?;
                    flow = cflow;
                    statements.extend(s);
                }
                ast::Statement::Return(expr) => {
                    match (expr, self.expected_return_ty) {
                        (Some(expr), Some(expected_return_ty)) => {
                            let expr = self.build_expression(expr, Some(expected_return_ty))?;
                            statements.push(Statement::Return(Some(expr)));
                        }
                        (None, None) => {
                            statements.push(Statement::Return(None));
                        }
                        _ => {
                            return Err(SemanticError::ReturnTypeMismatch {
                                expected: self.expected_return_ty,
                            }
                            .with_span(Span::default()));
                        }
                    }
                    flow = ControlFlow::Return;
                    if iter.next().is_some() {
                        println!("oops someone left some statements after the return");
                    }
                    break;
                }
                ast::Statement::LocalVar(identifier, ty, expr) => {
                    let explicit_ty = match ty {
                        Some(ast_ty) => Some(ast_ty.try_into()?),
                        None => None,
                    };

                    let identifier = identifier.unwrap();
                    let (expr, ty) = match (expr, explicit_ty) {
                        (None, Some(ty)) => (None, ty),
                        (Some(expr), Some(ty)) => {
                            let expr = self.build_expression(expr, Some(ty))?;
                            (Some(expr), ty)
                        }
                        (Some(expr), None) => {
                            let expr = self.build_expression(expr, None)?;
                            let ty = self.symbol_table.get_expression_type(&expr);
                            (Some(expr), ty)
                        }
                        (None, None) => {
                            return Err(
                                SemanticError::LocalVarMissingType.with_span(Span::default())
                            );
                        }
                    };

                    let variable = Variable { identifier, ty };
                    let variable_id = self.symbol_table.add_variable(variable);

                    let mut body = Vec::new();
                    if let Some(expr) = expr {
                        body.push(Statement::Assignment(
                            LValue::LocalVar(variable_id),
                            Box::new(expr),
                        ));
                    }

                    self.stack.push(variable_id);
                    let (stmts, cflow) = self.build_statements(iter)?;
                    flow = cflow;
                    body.extend(stmts.into_iter());
                    self.stack.pop();

                    let scope = Scope { variable_id, body };

                    statements.push(Statement::Scope(scope));
                    break;
                }
            }

            if let ControlFlow::Return = flow {
                break;
            }
        }

        Ok((statements, flow))
    }
}

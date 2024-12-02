use std::str::FromStr;

use crate::ast;

use super::{
    error::SemanticError,
    expression::{Expression, ExpressionKind, LValue},
    operator::UnaryOperator,
    primitive::Primitive,
    symbol::{LocalVarID, SymbolTable, Variable},
    types::{FloatTy, IntBitWidth, IntegerTy, TypeKind},
    FunctionDeclaration, FunctionDefinition, Module, Parameter, Scope, Statement,
};

#[derive(Default)]
pub struct Analyzer {
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

impl Analyzer {
    pub fn build_module(&mut self, module: ast::Module) -> Result<Module, SemanticError> {
        let mut declarations = Vec::new();
        for s in module.fn_declarations {
            let s = self.build_declaration(s)?;
            declarations.push(s);
        }

        let mut functions = Vec::new();
        for s in module.fn_definitions {
            let s = self.build_definition(s)?;
            functions.push(s);
        }

        Ok(Module {
            functions,
            declarations,
        })
    }

    fn build_statements<I: IntoIterator<Item = ast::Statement>>(
        &mut self,
        iter: I,
        stack: &mut Vec<LocalVarID>,
        expected_return_ty: Option<TypeKind>,
    ) -> Result<Vec<Statement>, SemanticError> {
        let mut iter = iter.into_iter();
        let mut statements = Vec::new();

        while let Some(statement) = iter.next() {
            match statement {
                ast::Statement::Conditional(expr, true_block, else_block) => {
                    let expr = self.build_expression(expr, stack)?;

                    if !matches!(expr.ty, TypeKind::IntType(_)) {
                        return Err(SemanticError::NotLogic(expr.ty));
                    }

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
                    statements.push(Statement::Expression(self.build_expression(expr, stack)?))
                }
                ast::Statement::Loop(expr, s) => {
                    let expr = self.build_expression(expr, stack)?;

                    if !matches!(expr.ty, TypeKind::IntType(_)) {
                        return Err(SemanticError::NotLogic(expr.ty));
                    }

                    let s = self.build_statements(*s, stack, expected_return_ty)?;
                    statements.push(Statement::Loop(expr, s));
                }
                ast::Statement::Block(s) => {
                    let old_len = stack.len();
                    let s = self.build_statements(s, stack, expected_return_ty)?;
                    stack.truncate(old_len);
                    statements.extend(s);
                }
                ast::Statement::Return(expr) => {
                    if let Some(expr) = expr {
                        let expr = self.build_expression(expr, stack)?;
                        if expected_return_ty == None {
                            return Err(SemanticError::ReturnTypeMismatch {
                                expected: None,
                                recieved: Some(expr.ty),
                            });
                        }
                        statements.push(Statement::Return(Some(expr)));
                    } else {
                        if expected_return_ty != None {
                            return Err(SemanticError::ReturnTypeMismatch {
                                expected: expected_return_ty,
                                recieved: None,
                            });
                        }
                        statements.push(Statement::Return(None));
                    }
                }

                ast::Statement::LocalVar(identifier, ty, expr) => {
                    let ty = ty.expect("Implicit type not supported");
                    let primitive = Primitive::from_str(&ty).unwrap();
                    let ty: TypeKind = primitive.into();

                    if let Some(expr) = expr {
                        let expr = self.build_expression(expr, stack)?;
                        let expr_ty = expr.ty;

                        assert_eq!(ty, expr_ty);
                    };

                    let symbol = Variable { identifier, ty };

                    let symbol_id = self.symbol_table.push_local_var(symbol);
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

        return Ok(statements);
    }

    fn build_declaration(
        &mut self,
        prototype: ast::FunctionPrototype,
    ) -> Result<FunctionDeclaration, SemanticError> {
        let name = prototype.name;
        let ty = prototype
            .return_type
            .map(|ty| Primitive::from_str(&ty))
            .transpose()?
            .map(Into::into);

        let mut params = Vec::new();
        for p in prototype.parameters {
            let name = p.name;
            let ty = Primitive::from_str(&p.ty)?.into();
            let param = Parameter { name, ty };
            params.push(param);
        }

        Ok(FunctionDeclaration { name, ty, params })
    }

    fn build_definition(
        &mut self,
        definition: ast::FunctionDefinition,
    ) -> Result<FunctionDefinition, SemanticError> {
        let declaration = self.build_declaration(definition.prototype)?;

        let mut stack = Vec::new();
        let body = self.build_statements(definition.body, &mut stack, declaration.ty)?;

        Ok(FunctionDefinition { declaration, body })
    }

    fn build_expression(
        &mut self,
        expression: ast::Expression,
        stack: &[LocalVarID],
    ) -> Result<Expression, SemanticError> {
        match expression.kind {
            // Every integer literal is an u64
            ast::ExpressionKind::IntegerLiteral(i) => {
                let kind = ExpressionKind::IntegerLiteral(i);
                let ty = IntegerTy {
                    bits: IntBitWidth::I64,
                    sign: false,
                };
                let ty = TypeKind::IntType(ty);

                Ok(Expression { kind, ty })
            }

            // Every float literal is f64
            ast::ExpressionKind::FloatLiteral(f) => {
                let kind = ExpressionKind::FloatLiteral(f);
                let ty = TypeKind::FloatType(FloatTy::F64);

                Ok(Expression { kind, ty })
            }
            ast::ExpressionKind::StringLiteral(_) => {
                panic!("String literals are not supported yet")
            }

            // Every boolean literal is an u8
            ast::ExpressionKind::BooleanLiteral(b) => {
                let kind = ExpressionKind::IntegerLiteral(if b { 1 } else { 0 });
                let ty = IntegerTy {
                    bits: IntBitWidth::I8,
                    sign: false,
                };
                let ty = TypeKind::IntType(ty);

                Ok(Expression { kind, ty })
            }
            ast::ExpressionKind::Identifier(ident) => {
                for s in stack.iter().copied() {
                    let symbol = self.symbol_table.get(s);
                    if symbol.identifier == ident {
                        let ty = symbol.ty;
                        let kind = ExpressionKind::LValue(LValue::LocalVar(s));
                        let expr = Expression { ty, kind };
                        return Ok(expr);
                    }
                }

                Err(SemanticError::Undeclared(ident))
            }
            ast::ExpressionKind::BinaryOperation(left, op, right) => {
                let op = op.try_into()?;

                let left = self.build_expression(*left, stack)?;
                let right = self.build_expression(*right, stack)?;

                let ty = match (left.ty, right.ty) {
                    (TypeKind::IntType(l), TypeKind::IntType(r)) => {
                        let sign = l.sign || r.sign;
                        let bits = l.bits.max(r.bits);
                        TypeKind::IntType(IntegerTy { sign, bits })
                    }
                    (TypeKind::FloatType(l), TypeKind::FloatType(r)) => {
                        TypeKind::FloatType(l.max(r))
                    }
                    _ => {
                        return Err(SemanticError::TypeMismatch {
                            expected: left.ty,
                            recieved: Some(right.ty),
                        });
                    }
                };

                let kind = ExpressionKind::BinaryOperation(Box::new(left), op, Box::new(right));
                Ok(Expression { kind, ty })
            }
            ast::ExpressionKind::UnaryOperation(operator, expression) => {
                let operator = operator.try_into()?;
                let expression = self.build_expression(*expression, stack)?;
                let ty = match operator {
                    UnaryOperator::Negative => {
                        if let TypeKind::IntType(mut int_type) = expression.ty {
                            int_type.sign = true;
                            TypeKind::IntType(int_type)
                        } else {
                            expression.ty
                        }
                    }
                    UnaryOperator::LogicNot | UnaryOperator::BitNot
                        if matches!(expression.ty, TypeKind::IntType(_)) =>
                    {
                        expression.ty
                    }
                    _ => return Err(SemanticError::InvalidUnaryOp(operator, expression.ty)),
                };

                let kind = ExpressionKind::UnaryOperation(operator, Box::new(expression));

                Ok(Expression { kind, ty })
            }
            ast::ExpressionKind::FunctionCall(name, args) => {
                Err(SemanticError::FunctionCallsNotImplemented)
            }
        }
    }
}

// pub fn cast_to_int(&mut self, expr: ExpressionEnum) -> Result<IntExpression, SemanticError> {
//     match expr {
//         ExpressionEnum::IntExpression(expr) => Ok(expr),
//         ExpressionEnum::FloatExpression(expr) => Ok(IntExpression::Cast {
//             ty: IntegerTy {
//                 bits: IntBitWidth::I8,
//                 sign: false,
//             },
//             expr: Box::new(ExpressionEnum::FloatExpression(expr)),
//         }),
//     }
// }
//

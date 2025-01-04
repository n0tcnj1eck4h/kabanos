use crate::{
    ast,
    span::{HasSpan, Span, Spanned, WithSpan as _},
    token::Operator,
};

use super::{
    error::SemanticError,
    expression::{ExpressionKind, LValue},
    operator::{BinaryOperator, UnaryOperator},
    symbol::{SymbolTable, VariableID},
    types::{FloatTy, IntBitWidth, IntegerTy, TypeKind},
    FunctionCall,
};

#[derive(Debug, Clone)]
struct InnerExpression {
    pub kind: ExpressionKind,
    pub ty: TypeKind,
}

impl InnerExpression {
    fn with_span(self, span: Span) -> super::Expression {
        super::Expression {
            kind: self.kind,
            ty: self.ty,
            span,
        }
    }
}

pub struct ExpressionBuilder<'a> {
    pub stack: &'a [VariableID],
    pub symbol_table: &'a SymbolTable,
    pub expected_ty: Option<TypeKind>,
}

impl ExpressionBuilder<'_> {
    pub fn build_expression(
        &self,
        expression: Spanned<ast::Expression>,
    ) -> Result<super::Expression, Spanned<SemanticError>> {
        let span = expression.get_span();
        let expr = self
            .build_inner_expression(expression.unwrap())
            .map_err(|e| e.with_span(span))?;

        Ok(super::Expression {
            kind: expr.kind,
            ty: expr.ty,
            span,
        })
    }

    fn build_inner_expression(
        &self,
        expression: ast::Expression,
    ) -> Result<InnerExpression, SemanticError> {
        let expr = match expression {
            ast::Expression::IntegerLiteral(i) => self.build_int_literal(i),
            ast::Expression::FloatLiteral(f) => self.build_float_literal(f),
            ast::Expression::BooleanLiteral(b) => self.build_boolean_literal(b),
            ast::Expression::Identifier(ident) => self.build_identifier(ident),
            ast::Expression::BinaryOp(left, op, right) => self.build_binop(*left, op, *right),
            ast::Expression::UnaryOperation(op, expr) => self.build_unary_operation(op, *expr),
            ast::Expression::FunctionCall(name, args) => self.build_function_call(name, args),
            ast::Expression::StringLiteral(_) => {
                panic!("String literals are not supported yet")
            }
        }?;

        // TODO: this crashes on while n + i {}
        if let Some(expected_ty) = self.expected_ty {
            assert_eq!(expr.ty, expected_ty);
        }

        Ok(expr)
    }

    fn build_expression_with_type(
        &self,
        expression: Spanned<ast::Expression>,
        expected_ty: Option<TypeKind>,
    ) -> Result<InnerExpression, SemanticError> {
        let builder = ExpressionBuilder {
            stack: self.stack,
            symbol_table: self.symbol_table,
            expected_ty,
        };
        builder.build_inner_expression(expression.unwrap())
    }

    fn build_float_literal(&self, f: f64) -> Result<InnerExpression, SemanticError> {
        let default_float_type = TypeKind::FloatType(FloatTy::F32);
        let ty = self.expected_ty.unwrap_or(default_float_type);

        let kind = ExpressionKind::FloatLiteral(f);
        if let TypeKind::FloatType(_) = ty {
            Ok(InnerExpression { kind, ty })
        } else {
            Err(SemanticError::TypeMismatch {
                expected: ty,
                found: default_float_type,
            })
        }
    }

    fn build_boolean_literal(&self, b: bool) -> Result<InnerExpression, SemanticError> {
        let kind = ExpressionKind::BooleanLiteral(b);
        let ty = self.expected_ty.unwrap_or(TypeKind::Boolean);
        if TypeKind::Boolean == ty {
            Ok(InnerExpression { kind, ty })
        } else {
            Err(SemanticError::TypeMismatch {
                expected: ty,
                found: TypeKind::Boolean,
            })
        }
    }

    fn build_identifier(&self, ident: String) -> Result<InnerExpression, SemanticError> {
        for s in self.stack.iter().rev().copied() {
            let symbol = self.symbol_table.get_variable(s);
            if symbol.identifier == ident {
                let ty = symbol.ty;
                if let Some(expected_ty) = self.expected_ty {
                    if expected_ty != ty {
                        return Err(SemanticError::TypeMismatch {
                            expected: expected_ty,
                            found: ty,
                        });
                    }
                }

                let kind = ExpressionKind::LValue(LValue::LocalVar(s));
                let expr = InnerExpression { ty, kind };
                return Ok(expr);
            }
        }

        Err(SemanticError::Undeclared(ident))
    }

    fn build_unary_operation(
        &self,
        op: Operator,
        expr: Spanned<ast::Expression>,
    ) -> Result<InnerExpression, SemanticError> {
        let span = expr.get_span();
        let operator = op.try_into()?;
        let expression = self.build_inner_expression(expr.unwrap())?;
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

        let kind = ExpressionKind::UnaryOperation(operator, Box::new(expression.with_span(span)));
        Ok(InnerExpression { kind, ty })
    }

    fn build_function_call(
        &self,
        name: String,
        args: Vec<Spanned<ast::Expression>>,
    ) -> Result<InnerExpression, SemanticError> {
        let fn_id = self
            .symbol_table
            .get_function_id_by_name(&name)
            .ok_or(SemanticError::Undeclared(name))?;

        let fn_decl = self.symbol_table.get_function(fn_id);
        if let Some(expected_ty) = self.expected_ty {
            let Some(fn_ty) = fn_decl.ty else {
                return Err(SemanticError::VoidOperation);
            };
            if fn_ty != expected_ty {
                return Err(SemanticError::TypeMismatch {
                    expected: expected_ty,
                    found: fn_ty,
                });
            }
        }

        let ty = fn_decl.ty.ok_or(SemanticError::VoidOperation)?;
        let params = &fn_decl.params;
        if params.len() != args.len() {
            return Err(SemanticError::WrongArgumentCount);
        }

        let args: Result<Vec<_>, _> = args
            .into_iter()
            .zip(params)
            .map(|(arg, param)| {
                let span = arg.get_span();
                let expr = self.build_expression_with_type(arg, Some(param.ty))?;
                Ok(super::Expression {
                    kind: expr.kind,
                    ty: expr.ty,
                    span,
                })
            })
            .collect();

        let call = FunctionCall {
            id: fn_id,
            args: args?,
        };
        let kind = ExpressionKind::FunctionCall(call);
        Ok(InnerExpression { ty, kind })
    }

    fn build_int_literal(&self, i: u64) -> Result<InnerExpression, SemanticError> {
        let kind = ExpressionKind::IntegerLiteral(i);
        let default_int_type = TypeKind::IntType(IntegerTy {
            bits: IntBitWidth::I64,
            sign: false,
        });

        let ty = self.expected_ty.unwrap_or(default_int_type);

        if let TypeKind::IntType(_) = ty {
            Ok(InnerExpression { kind, ty })
        } else {
            Err(SemanticError::TypeMismatch {
                expected: ty,
                found: default_int_type,
            })
        }
    }

    fn build_binop(
        &self,
        left: Spanned<ast::Expression>,
        op: Operator,
        right: Spanned<ast::Expression>,
    ) -> Result<InnerExpression, SemanticError> {
        let left_span = left.get_span();
        let right_span = right.get_span();

        if op == Operator::As {
            let ast::Expression::Identifier(ident) = right.unwrap() else {
                return Err(SemanticError::BadCast);
            };

            let ty: TypeKind = ident.parse()?;
            self.assert_expected_ty(ty)?;

            let left = self.build_expression_with_type(left, None)?;
            let kind = ExpressionKind::Cast(Box::new(left.with_span(left_span)), ty);
            return Ok(InnerExpression { kind, ty });
        }

        let (left, operand_ty, right) = {
            if right.is_strongly_typed() {
                let right = self.build_expression_with_type(right, None)?;
                let left = self.build_expression_with_type(left, Some(right.ty))?;
                (left, right.ty, right)
            } else {
                let left = self.build_expression_with_type(left, None)?;
                let ty = left.ty;
                let right = self.build_expression_with_type(right, Some(left.ty))?;
                (left, ty, right)
            }
        };

        if op == Operator::Assign {
            let ExpressionKind::LValue(LValue::LocalVar(variable_id)) = left.kind else {
                return Err(SemanticError::LValue(left.with_span(left_span)));
            };

            let ty = left.ty;
            self.assert_expected_ty(ty)?;

            let kind = ExpressionKind::Assignment(
                LValue::LocalVar(variable_id),
                Box::new(right.with_span(right_span)),
            );
            return Ok(InnerExpression { kind, ty });
        }

        let op: BinaryOperator = op.try_into()?;
        let result_ty = op.get_result_ty(operand_ty)?;
        self.assert_expected_ty(result_ty)?;

        let kind = ExpressionKind::BinaryOperation(
            Box::new(left.with_span(left_span)),
            op,
            Box::new(right.with_span(right_span)),
        );
        Ok(InnerExpression {
            kind,
            ty: result_ty,
        })
    }

    fn assert_expected_ty(&self, ty: TypeKind) -> Result<(), SemanticError> {
        if let Some(expected_ty) = self.expected_ty {
            if ty != expected_ty {
                return Err(SemanticError::TypeMismatch {
                    expected: expected_ty,
                    found: ty,
                });
            }
        }
        Ok(())
    }
}

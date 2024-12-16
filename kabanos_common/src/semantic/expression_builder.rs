use crate::{ast, span::Span, token::Operator};

use super::{
    error::{SemanticError, SemanticErrorKind},
    expression::{ExpressionKind, LValue},
    operator::{BinaryOperator, UnaryOperator},
    symbol::{SymbolTable, VariableID},
    types::{FloatTy, IntBitWidth, IntegerTy, TypeKind},
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
        expression: ast::Expression,
    ) -> Result<super::Expression, SemanticError> {
        let span = expression.span;
        let expr = self
            .build_inner_expression(expression)
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
    ) -> Result<InnerExpression, SemanticErrorKind> {
        let expr = match expression.kind {
            ast::ExpressionKind::IntegerLiteral(i) => self.build_int_literal(i),
            ast::ExpressionKind::FloatLiteral(f) => self.build_float_literal(f),
            ast::ExpressionKind::BooleanLiteral(b) => self.build_boolean_literal(b),
            ast::ExpressionKind::Identifier(ident) => self.build_identifier(ident),
            ast::ExpressionKind::BinaryOp(left, op, right) => self.build_binop(*left, op, *right),
            ast::ExpressionKind::UnaryOperation(op, expr) => self.build_unary_operation(op, *expr),
            ast::ExpressionKind::FunctionCall(name, args) => self.build_function_call(name, args),
            ast::ExpressionKind::StringLiteral(_) => {
                panic!("String literals are not supported yet")
            }
        }?;

        if let Some(expected_ty) = self.expected_ty {
            assert_eq!(expr.ty, expected_ty);
        }

        Ok(expr)
    }

    fn build_expression_with_type(
        &self,
        expression: ast::Expression,
        expected_ty: Option<TypeKind>,
    ) -> Result<InnerExpression, SemanticErrorKind> {
        let builder = ExpressionBuilder {
            stack: self.stack,
            symbol_table: self.symbol_table,
            expected_ty,
        };
        builder.build_inner_expression(expression)
    }

    fn build_float_literal(&self, f: f64) -> Result<InnerExpression, SemanticErrorKind> {
        let default_float_type = TypeKind::FloatType(FloatTy::F32);
        let ty = self.expected_ty.unwrap_or(default_float_type);

        let kind = ExpressionKind::FloatLiteral(f);
        if let TypeKind::FloatType(_) = ty {
            Ok(InnerExpression { kind, ty })
        } else {
            Err(SemanticErrorKind::TypeMismatch {
                expected: default_float_type,
                found: ty,
            })
        }
    }

    fn build_boolean_literal(&self, b: bool) -> Result<InnerExpression, SemanticErrorKind> {
        let kind = ExpressionKind::BooleanLiteral(b);
        let ty = self.expected_ty.unwrap_or(TypeKind::Boolean);
        if TypeKind::Boolean == ty {
            Ok(InnerExpression { kind, ty })
        } else {
            Err(SemanticErrorKind::TypeMismatch {
                expected: TypeKind::Boolean,
                found: ty,
            })
        }
    }

    fn build_identifier(&self, ident: String) -> Result<InnerExpression, SemanticErrorKind> {
        for s in self.stack.iter().rev().copied() {
            let symbol = self.symbol_table.get_variable(s);
            if symbol.identifier == ident {
                let ty = symbol.ty;
                if let Some(expected_ty) = self.expected_ty {
                    if expected_ty != ty {
                        return Err(SemanticErrorKind::TypeMismatch {
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

        Err(SemanticErrorKind::Undeclared(ident))
    }

    fn build_unary_operation(
        &self,
        op: Operator,
        expr: ast::Expression,
    ) -> Result<InnerExpression, SemanticErrorKind> {
        let span = expr.span;
        let operator = op.try_into()?;
        let expression = self.build_inner_expression(expr)?;
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
            _ => return Err(SemanticErrorKind::InvalidUnaryOp(operator, expression.ty)),
        };

        let kind = ExpressionKind::UnaryOperation(operator, Box::new(expression.with_span(span)));
        Ok(InnerExpression { kind, ty })
    }

    fn build_function_call(
        &self,
        name: String,
        args: Vec<ast::Expression>,
    ) -> Result<InnerExpression, SemanticErrorKind> {
        let fn_id = self
            .symbol_table
            .get_function_id_by_name(&name)
            .ok_or(SemanticErrorKind::Undeclared(name))?;

        let fn_decl = self.symbol_table.get_function(fn_id);
        if let Some(expected_ty) = self.expected_ty {
            let fn_ty = fn_decl.ty.unwrap();
            if fn_ty != expected_ty {
                return Err(SemanticErrorKind::TypeMismatch {
                    expected: expected_ty,
                    found: fn_ty,
                });
            }
        }

        let ty = fn_decl.ty.ok_or(SemanticErrorKind::VoidOperation)?;
        let params = &fn_decl.params;
        if params.len() != args.len() {
            return Err(SemanticErrorKind::WrongArgumentCount);
        }

        let args: Result<Vec<_>, _> = args
            .into_iter()
            .zip(params)
            .map(|(arg, param)| {
                let span = arg.span;
                let expr = self.build_expression_with_type(arg, Some(param.ty))?;
                Ok(super::Expression {
                    kind: expr.kind,
                    ty: expr.ty,
                    span,
                })
            })
            .collect();

        let kind = ExpressionKind::FunctionCall(fn_id, args?);
        Ok(InnerExpression { ty, kind })
    }

    fn build_int_literal(&self, i: u64) -> Result<InnerExpression, SemanticErrorKind> {
        let kind = ExpressionKind::IntegerLiteral(i);
        let default_int_type = TypeKind::IntType(IntegerTy {
            bits: IntBitWidth::I64,
            sign: false,
        });

        let ty = self.expected_ty.unwrap_or(default_int_type);

        if let TypeKind::IntType(_) = ty {
            Ok(InnerExpression { kind, ty })
        } else {
            Err(SemanticErrorKind::TypeMismatch {
                expected: default_int_type,
                found: ty,
            })
        }
    }

    fn build_binop(
        &self,
        left: ast::Expression,
        op: Operator,
        right: ast::Expression,
    ) -> Result<InnerExpression, SemanticErrorKind> {
        let op = op.try_into()?;
        let left_span = left.span;
        let right_span = right.span;

        // TODO uhh
        let left = self.build_expression_with_type(left, None)?;
        let ty = left.ty;
        let right = self.build_expression_with_type(right, Some(left.ty))?;

        use BinaryOperator::*;
        let (left, op, right, ty) = match op {
            Equal | Less | Greater | LessOrEqual | GreaterOrEqual | NotEqual => {
                let ty = TypeKind::Boolean;
                (left, op, right, ty)
            }

            Add | Subtract | Multiply | Divide | Modulo => {
                if let TypeKind::IntType(_) | TypeKind::FloatType(_) = ty {
                    (left, op, right, ty)
                } else {
                    return Err(SemanticErrorKind::InvalidBinOp);
                }
            }

            BitAnd | BitOr | BitXor | BitLeft | BitRight => {
                if let TypeKind::IntType(_) = ty {
                    (left, op, right, ty)
                } else {
                    return Err(SemanticErrorKind::InvalidBinOp);
                }
            }

            LogicAnd | LogicOr => {
                if let TypeKind::Boolean = ty {
                    (left, op, right, ty)
                } else {
                    return Err(SemanticErrorKind::InvalidBinOp);
                }
            }

            Assign => {
                if let ExpressionKind::LValue(lvalue) = left.kind {
                    let kind =
                        ExpressionKind::Assignment(lvalue, Box::new(right.with_span(right_span)));
                    return Ok(InnerExpression { kind, ty });
                } else {
                    return Err(SemanticErrorKind::LValue(left.with_span(left_span)));
                }
            }
        };

        let kind = ExpressionKind::BinaryOperation(
            Box::new(left.with_span(left_span)),
            op,
            Box::new(right.with_span(right_span)),
        );
        Ok(InnerExpression { kind, ty })
    }
}

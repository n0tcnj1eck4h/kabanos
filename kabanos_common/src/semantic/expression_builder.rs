use crate::{
    ast,
    span::{HasSpan, Spanned, WithSpan as _},
    token::Operator,
};

use super::{
    error::SemanticError,
    expression::{Expression, LValue},
    operator::{BinaryOperator, UnaryOperator},
    statement_builder::Analyzer,
    types::{FloatTy, IntTy, Type, TypeKind},
    FunctionCall,
};

impl Analyzer<'_> {
    pub fn build_expression(
        &self,
        expression: Spanned<ast::Expression>,
        expected_ty: Option<Type>,
    ) -> Result<super::Expression, Spanned<SemanticError>> {
        let span = expression.get_span();
        let expr = self
            .build_inner_expression(expression.unwrap(), expected_ty)
            .map_err(|e| e.with_span(span))?;

        Ok(expr)
    }

    fn build_inner_expression(
        &self,
        expression: ast::Expression,
        expected_ty: Option<Type>,
    ) -> Result<Expression, SemanticError> {
        let expr = match expression {
            ast::Expression::BooleanLiteral(b) => Ok(Expression::BooleanLiteral(b)),
            ast::Expression::IntegerLiteral(i) => self.build_int_literal(i, expected_ty),
            ast::Expression::FloatLiteral(f) => self.build_float_literal(f, expected_ty),
            ast::Expression::Identifier(ident) => self.build_identifier(ident),
            ast::Expression::BinaryOp(left, op, right) => {
                self.build_binop(*left, op, *right, expected_ty)
            }
            ast::Expression::UnaryOperation(op, expr) => {
                self.build_unary_op(op, *expr, expected_ty)
            }
            ast::Expression::FunctionCall(name, args) => self.build_function_call(name, args),
            ast::Expression::StringLiteral(s) => Ok(Expression::StringLiteral(s)),
            ast::Expression::Cast(expr, ty) => self.build_cast(*expr, ty),
        }?;

        if let Some(expected) = expected_ty {
            let expr_ty = self.symbol_table.get_expression_type(&expr);
            if expr_ty != expected {
                return Err(SemanticError::TypeMismatch {
                    expected: expected.clone(),
                    found: expr_ty,
                });
            }
        }

        Ok(expr)
    }

    fn build_cast(
        &self,
        expr: Spanned<ast::Expression>,
        ty: ast::Type,
    ) -> Result<Expression, SemanticError> {
        let ty: Type = ty.try_into()?;
        let expr = self.build_inner_expression(expr.unwrap(), None)?;
        Ok(Expression::Cast(Box::new(expr), ty.clone()))
    }

    fn build_float_literal(
        &self,
        f: f64,
        prefered_type: Option<Type>,
    ) -> Result<Expression, SemanticError> {
        let float_type = match prefered_type {
            None => FloatTy::F32,
            Some(Type {
                kind: TypeKind::Float(float_ty),
                ptr_depth: 0,
            }) => float_ty,
            Some(ty) => {
                return Err(SemanticError::TypeMismatch {
                    expected: ty.clone(),
                    found: TypeKind::Float(FloatTy::F32).into(),
                })
            }
        };

        Ok(Expression::FloatLiteral(f, float_type.into()))
    }

    fn build_identifier(&self, ident: String) -> Result<Expression, SemanticError> {
        for s in self.stack.iter().rev().copied() {
            let symbol = self.symbol_table.get_variable(s);
            if symbol.identifier == ident {
                return Ok(Expression::LValue(LValue::LocalVar(s)));
            }
        }

        Err(SemanticError::Undeclared(ident))
    }

    fn build_unary_op(
        &self,
        op: Operator,
        expr: Spanned<ast::Expression>,
        prefered_type: Option<Type>,
    ) -> Result<Expression, SemanticError> {
        let operator = op.try_into()?;

        if let Some(mut prefered_type) = prefered_type {
            let prefered_operand_type = match (operator, prefered_type.kind) {
                (UnaryOperator::Ref, _) if prefered_type.is_ptr() => {
                    prefered_type.ptr_depth -= 1;
                    prefered_type
                }
                (UnaryOperator::Deref, _) => {
                    prefered_type.ptr_depth += 1;
                    prefered_type
                }
                (UnaryOperator::Negative, ty @ TypeKind::Int(_))
                | (UnaryOperator::Negative, ty @ TypeKind::Float(_))
                | (UnaryOperator::LogicNot, ty @ TypeKind::Bool)
                | (UnaryOperator::BitNot, ty @ TypeKind::Int(_)) => ty.into(),
                (_, ty) => return Err(SemanticError::InvalidUnaryOp(operator, ty.into())),
            };

            let expr = self.build_inner_expression(expr.unwrap(), Some(prefered_operand_type))?;
            Ok(Expression::UnaryOperation(operator, Box::new(expr)))
        } else {
            let expr = self.build_inner_expression(expr.unwrap(), None)?;
            Ok(Expression::UnaryOperation(operator, Box::new(expr)))
        }
    }

    fn build_function_call(
        &self,
        name: String,
        args: Vec<Spanned<ast::Expression>>,
    ) -> Result<Expression, SemanticError> {
        let id = self
            .symbol_table
            .get_function_id_by_name(&name)
            .ok_or(SemanticError::Undeclared(name))?;

        let fn_decl = self.symbol_table.get_function(id);
        fn_decl.ty.as_ref().ok_or(SemanticError::VoidOperation)?;

        if fn_decl.params.len() != args.len() {
            return Err(SemanticError::WrongArgumentCount);
        }

        let args = args
            .into_iter()
            .zip(&fn_decl.params)
            .map(|(arg, param)| self.build_inner_expression(arg.unwrap(), Some(param.ty)))
            .collect::<Result<Vec<_>, _>>()?;

        let call = FunctionCall { id, args };
        Ok(Expression::FunctionCall(call))
    }

    fn build_int_literal(
        &self,
        i: u64,
        prefered_type: Option<Type>,
    ) -> Result<Expression, SemanticError> {
        let default_int_type = IntTy::I32;

        let int_type = match prefered_type {
            Some(Type {
                kind: TypeKind::Int(int_ty),
                ptr_depth: 0,
            }) => int_ty,
            None => default_int_type,
            Some(ty) => {
                return Err(SemanticError::TypeMismatch {
                    expected: ty.clone(),
                    found: default_int_type.into(),
                })
            }
        };

        Ok(Expression::IntegerLiteral(i, int_type))
    }

    fn build_binop(
        &self,
        left: Spanned<ast::Expression>,
        op: Operator,
        right: Spanned<ast::Expression>,
        prefered_type: Option<Type>,
    ) -> Result<Expression, SemanticError> {
        let binop: BinaryOperator = op.try_into()?;
        let prefered_type = if let Some(ty) = prefered_type {
            if ty.is_ptr() {
                return Err(SemanticError::PointerArithmetic);
            }
            match (binop, ty.kind) {
                (BinaryOperator::Logic(_), TypeKind::Bool) => None,
                (BinaryOperator::Comparaison(_), TypeKind::Bool) => None,
                (BinaryOperator::Bitwise(_), TypeKind::Int(_)) => Some(ty),
                (BinaryOperator::Arithmetic(_), TypeKind::Int(_)) => Some(ty),
                (BinaryOperator::Arithmetic(_), TypeKind::Float(_)) => Some(ty),
                _ => return Err(SemanticError::InvalidBinOp),
            }
        } else {
            None
        };

        if left.is_strongly_typed() {
            let left = self.build_inner_expression(left.unwrap(), prefered_type)?;
            let ty = self.symbol_table.get_expression_type(&left);
            let right = self.build_inner_expression(right.unwrap(), Some(ty))?;
            Ok(Expression::BinaryOperation(
                Box::new(left),
                binop,
                Box::new(right),
            ))
        } else {
            let right = self.build_inner_expression(right.unwrap(), prefered_type)?;
            let ty = self.symbol_table.get_expression_type(&right);
            let left = self.build_inner_expression(left.unwrap(), Some(ty))?;
            Ok(Expression::BinaryOperation(
                Box::new(left),
                binop,
                Box::new(right),
            ))
        }
    }
}

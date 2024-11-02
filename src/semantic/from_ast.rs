use std::{collections::VecDeque, str::FromStr};

use crate::ast;

use super::{
    error::SemanticError,
    expression::{ExpressionEnum, IntExpression},
    primitive::Primitive,
    symbol::{SymbolID, SymbolTable},
    types::{FloatType, IntBitWidths, IntegerType, TypeEnum},
    FunctionDeclaration, FunctionDefinition, Module, Parameter, Scope, Statement,
};

#[derive(Default)]
pub struct Context<S> {
    symbol_table: SymbolTable,
    stmt_iter: Option<S>,
    expected_return_type: Option<TypeEnum>,
    scope_stack: VecDeque<SymbolID>,
}

impl<S> Context<S>
where
    S: Iterator<Item = ast::Statement>,
{
    pub fn build_module(&mut self, module: ast::Module) -> Result<Module, SemanticError> {
        let mut declarations = Vec::new();
        for s in module.function_declarations {
            let s = self.build_declaration(s)?;
            declarations.push(s);
        }

        let mut functions = Vec::new();
        for s in module.function_definitions {
            let s = s.build_semantic(symbol_table)?;
            functions.push(s);
        }

        Ok(Module {
            functions,
            declarations,
        })
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
        self.expected_return_type = declaration.ty;

        let mut scope = Scope::default();
        for statement in definition.body {
            scope.body.push(statement.build_semantic(symbol_table)?);
        }

        Ok(FunctionDefinition {
            declaration,
            body: scope,
        })
    }

    fn build_next_statement(&mut self) -> Result<Option<Statement>, SemanticError> {
        let Some(statement) = self.stmt_iter.unwrap().next() else {
            return Ok(None);
        };

        match statement {
            ast::Statement::Conditional(expr, true_block, else_block) => {
                let expr = self.build_expression(expr)?;
                let expr = self.cast_to_int(expr)?;

                let true_block = Box::new(self.build_declaration);

                let else_block = else_block
                    .map(|eb| (*eb).build_semantic(symbol_table))
                    .transpose()?
                    .map(Box::new);

                Ok(Statement::Conditional(expr, true_block, else_block))
            }
            ast::Statement::Expression(expr) => {
                Ok(Statement::Expression(expr.build_semantic(symbol_table)?))
            }
            ast::Statement::Loop(expr, statement) => {
                let expr = expr
                    .build_semantic(symbol_table)?
                    .cast_to_int(symbol_table)?;

                let statement = Box::new((*statement).build_semantic(symbol_table)?);

                Ok(Statement::Loop(expr, statement))
            }
            ast::Statement::Block(statements) => {
                symbol_table.push_scope();

                let mut body = Vec::new();
                for s in statements {
                    let s = s.build_semantic(symbol_table)?;
                    body.push(s);
                }

                let local_symbols = symbol_table.pop_scope().values().copied().collect();
                let scope = Scope {
                    local_symbols,
                    body,
                };

                Ok(Statement::Block(scope))
            }
            ast::Statement::Return(expr) => Ok(Statement::Return(
                // TODO: return type???
                expr.map(|e| e.build_semantic(symbol_table)).transpose()?,
            )),
            ast::Statement::LocalVar(identifier, ty, expr) => {
                let ty = ty.expect("Implicit type not supported");
                let primitive = Primitive::from_str(&ty)?;
                let ty: TypeEnum = primitive.into();

                if let Some(expr) = expr {
                    let expr = expr.build_semantic(symbol_table)?;
                    let expr_ty = expr.get_type(symbol_table);

                    assert_eq!(ty, expr_ty);
                };

                let symbol = Symbol {
                    name: identifier,
                    ty,
                };

                self.symbol_table.push_scope();
                self.symbol_table.push_local_symbol(symbol);
                // change to iterator over ast::statement
                // move scope stack out of symbol table
            }
        }
    }

    fn build_expression(
        &mut self,
        expression: ast::Expression,
    ) -> Result<ExpressionEnum, SemanticError> {
        todo!()
    }

    pub fn cast_to_int(&mut self, expr: ExpressionEnum) -> Result<IntExpression, SemanticError> {
        match expr {
            ExpressionEnum::IntExpression(expr) => Ok(expr),
            ExpressionEnum::FloatExpression(expr) => Ok(IntExpression::Cast {
                ty: IntegerType {
                    bit_width: IntBitWidths::I8,
                    signed: false,
                },
                expr: Box::new(ExpressionEnum::FloatExpression(expr)),
            }),
        }
    }

    pub fn get_expression_type(&self, expr: &ExpressionEnum) -> TypeEnum {
        match expr {
            ExpressionEnum::IntExpression(int_expr) => TypeEnum::IntType(self.get_type(int_expr)),
            // TODO:
            ExpressionEnum::FloatExpression(_) => TypeEnum::FloatType(FloatType::F32),
        }
    }

    fn get_int_expr_type(&self, expr: &IntExpression) -> IntegerType {
        use IntExpression::*;
        match expr {
            Cast { ty, expr } => *ty,
            LValue(_) => todo!(),
            IntegerLiteral(_) => IntegerType {
                bit_width: IntBitWidths::I64,
                signed: false,
            },
            BooleanLiteral(_) => IntegerType {
                bit_width: IntBitWidths::I8,
                signed: false,
            },
            UnaryOperation(UnaryOperator::Negative, expr) => {
                let mut ty = expr.get_type(symbol_table);
                ty.signed = true;
                ty
            }
            UnaryOperation(_, expr) => expr.get_type(symbol_table),
            FunctionCall(_, _) => todo!(),
            Assignment(_, expr) => expr.get_type(symbol_table),
            BinaryOperation(l, op, r) => {
                let l = l.get_type(symbol_table);
                let r = r.get_type(symbol_table);
                let bit_width = l.bit_width.max(r.bit_width);
                let signed = l.signed;
                IntegerType { bit_width, signed }
            }
        }
    }
}

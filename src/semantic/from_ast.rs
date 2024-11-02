use std::str::FromStr;

use crate::{ast, semantic::operator::UnaryOperator};

use super::{
    error::SemanticError,
    expression::{ExpressionEnum, IntExpression},
    primitive::Primitive,
    symbol::{Symbol, SymbolID, SymbolTable},
    types::{FloatType, IntBitWidths, IntegerType, TypeEnum},
    FunctionDeclaration, FunctionDefinition, Module, Parameter, Scope, Statement,
};

#[derive(Default)]
pub struct Context {
    symbol_table: SymbolTable,
}

// struct StatementBuilder<'ctx> {
//     context: &'ctx mut Context,
//     stmt_iter_stack: Vec<Box<dyn Iterator<Item = ast::Statement> + 'ctx>>,
//     expected_return_type: Option<TypeEnum>,
// }
//
// impl<'ctx> StatementBuilder<'ctx> {
//     fn new<I: IntoIterator<Item = ast::Statement> + 'ctx>(
//         statement_iter: I,
//         context: &'ctx mut Context,
//         expected_return_type: Option<TypeEnum>,
//     ) -> Self {
//         let ptr: Box<dyn Iterator<Item = ast::Statement>> = Box::new(statement_iter.into_iter());
//
//         let mut stmt_iter_stack = Vec::new();
//         stmt_iter_stack.push(ptr);
//         Self {
//             context,
//             stmt_iter_stack,
//             expected_return_type,
//         }
//     }
//
//     fn nest<'a: 'ctx, I: IntoIterator<Item = ast::Statement> + 'ctx>(
//         &'a mut self,
//         iter: I,
//     ) -> Self {
//         Self {
//             context: self.context,
//             stmt_iter_stack: vec![Box::new(iter.into_iter())],
//             expected_return_type: self.expected_return_type,
//         }
//     }
// }
//
// impl<'ctx> Iterator for StatementBuilder<'ctx> {
//     type Item = Result<Statement, SemanticError>;
//     fn next(&mut self) -> Option<Self::Item> {
//         let statement = if let Some(top_iter) = self.stmt_iter_stack.last_mut() {
//             if let Some(statement) = top_iter.next() {
//                 statement
//             } else {
//                 self.stmt_iter_stack.pop();
//                 return self.next();
//             }
//         } else {
//             return None;
//         };
//     }AHAHAHAHAH
// }

impl Context {
    pub fn build_module(&mut self, module: ast::Module) -> Result<Module, SemanticError> {
        let mut declarations = Vec::new();
        for s in module.function_declarations {
            let s = self.build_declaration(s)?;
            declarations.push(s);
        }

        let mut functions = Vec::new();
        for s in module.function_definitions {
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
        stack: &mut Vec<SymbolID>,
    ) -> Result<Vec<Statement>, SemanticError> {
        let mut iter = iter.into_iter();
        let mut statements = Vec::new();

        while let Some(statement) = iter.next() {
            match statement {
                ast::Statement::Conditional(expr, true_block, else_block) => {
                    let expr = self.build_expression(expr)?;
                    let expr = self.cast_to_int(expr)?;

                    let true_block = self.build_statements(true_block, stack)?;

                    if let Some(else_block) = else_block {
                        let else_block = self.build_statements(else_block, stack)?;
                        statements.push(Statement::Conditional(expr, true_block, Some(else_block)));
                    } else {
                        statements.push(Statement::Conditional(expr, true_block, None));
                    }
                }
                ast::Statement::Expression(expr) => {
                    statements.push(Statement::Expression(self.build_expression(expr)?))
                }
                ast::Statement::Loop(expr, s) => {
                    let expr = self.build_expression(expr)?;
                    let expr = self.cast_to_int(expr)?;
                    let s = self.build_statements(s, stack)?;
                    statements.push(Statement::Loop(expr, s));
                }
                ast::Statement::Block(s) => {
                    let old_len = stack.len();
                    let s = self.build_statements(s, stack)?;
                    stack.truncate(old_len);
                    statements.extend(s);
                }
                ast::Statement::Return(expr) => {
                    if let Some(expr) = expr {
                        let expr = self.build_expression(expr)?;
                        statements.push(Statement::Return(Some(expr)));
                    } else {
                        statements.push(Statement::Return(None));
                    }
                }

                ast::Statement::LocalVar(identifier, ty, expr) => {
                    let ty = ty.expect("Implicit type not supported");
                    let primitive = Primitive::from_str(&ty).unwrap();
                    let ty: TypeEnum = primitive.into();

                    if let Some(expr) = expr {
                        let expr = self.build_expression(expr)?;
                        let expr_ty = self.get_expression_type(&expr);

                        assert_eq!(ty, expr_ty);
                    };

                    let symbol = Symbol { identifier, ty };

                    let symbol_id = self.symbol_table.push_local_symbol(symbol);
                    stack.push(symbol_id);
                    let s = self.build_statements(iter, stack)?;
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
        let body = self.build_statements(definition.body, &mut stack)?;

        Ok(FunctionDefinition { declaration, body })
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
            ExpressionEnum::IntExpression(int_expr) => {
                TypeEnum::IntType(self.get_int_expr_type(int_expr))
            }
            // TODO:
            ExpressionEnum::FloatExpression(_) => TypeEnum::FloatType(FloatType::F32),
        }
    }

    fn get_int_expr_type(&self, expr: &IntExpression) -> IntegerType {
        match expr {
            IntExpression::Cast { ty, .. } => *ty,
            IntExpression::LValue(symbol_id) => {
                let TypeEnum::IntType(ty) = self.symbol_table.get_symbol(*symbol_id).ty else {
                    panic!("int lvalue is not an actual int type oops")
                };
                ty
            }
            IntExpression::IntegerLiteral(_) => IntegerType {
                bit_width: IntBitWidths::I64,
                signed: false,
            },
            IntExpression::BooleanLiteral(_) => IntegerType {
                bit_width: IntBitWidths::I8,
                signed: false,
            },
            IntExpression::UnaryOperation(UnaryOperator::Negative, expr) => {
                let mut ty = self.get_int_expr_type(expr);
                ty.signed = true;
                ty
            }
            IntExpression::UnaryOperation(_, expr) => self.get_int_expr_type(expr),
            IntExpression::FunctionCall(_, _) => todo!(),
            IntExpression::Assignment(_, expr) => self.get_int_expr_type(expr),
            IntExpression::BinaryOperation(l, _, r) => {
                let l = self.get_int_expr_type(l);
                let r = self.get_int_expr_type(r);
                let bit_width = l.bit_width.max(r.bit_width);
                let signed = l.signed;
                IntegerType { bit_width, signed }
            }
        }
    }
}

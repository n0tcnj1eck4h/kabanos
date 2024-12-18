use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, FunctionValue, PointerValue},
};

use crate::semantic::{
    self,
    symbol::{SymbolTable, VariableID},
    Scope,
};

use super::error::CodegenResult;

pub struct StatementCodegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    function: FunctionValue<'ctx>,
    module: Module<'ctx>,
    variables: HashMap<VariableID, PointerValue<'ctx>>,
    symbol_table: &'ctx SymbolTable,
}

impl StatementCodegen<'_> {
    pub fn build_statement<'ctx>(&mut self, stmt: semantic::Statement) -> CodegenResult {
        use semantic::Statement::*;
        match stmt {
            Conditional(condition, body, body_else) => {
                let condition = self.build_expression(condition)?;

                let then_block = self.context.append_basic_block(self.function, "then");
                let else_block = self.context.append_basic_block(self.function, "else");
                let merge_block = self.context.append_basic_block(self.function, "merge");

                self.builder.build_conditional_branch(
                    condition.into_int_value(),
                    then_block,
                    else_block,
                )?;

                self.builder.position_at_end(then_block);
                for statement in body {
                    self.build_statement(statement)?;
                }
                self.builder.build_unconditional_branch(merge_block)?;

                self.builder.position_at_end(else_block);
                if let Some(body_else) = body_else {
                    for statement in body_else {
                        self.build_statement(statement)?;
                    }
                }
                self.builder.build_unconditional_branch(merge_block)?;

                self.builder.position_at_end(merge_block);
                Ok(())
            }
            Loop(condition, body) => {
                let loop_block = self.context.append_basic_block(self.function, "loop");
                let body_block = self.context.append_basic_block(self.function, "body");
                let continue_block = self.context.append_basic_block(self.function, "continue");

                self.builder.build_unconditional_branch(loop_block)?;
                self.builder.position_at_end(loop_block);

                let condition = self.build_expression(condition)?;

                self.builder.build_conditional_branch(
                    condition.into_int_value(),
                    body_block,
                    continue_block,
                )?;

                self.builder.position_at_end(body_block);
                for statement in body {
                    self.build_statement(statement)?;
                }
                self.builder.build_unconditional_branch(loop_block)?;

                self.builder.position_at_end(continue_block);
                Ok(())
            }
            Block(Scope { symbol, body }) => {
                let variable = self.symbol_table.get_variable(symbol);
                let ty = variable.ty.to_llvm_type(self.context);

                let ptr = self.builder.build_alloca(ty, &variable.identifier)?;
                self.variables.insert(symbol, ptr);
                // if let Some(expression) = value {
                //     let value = expression.build_expression(context, builder, symbol_table)?;
                //     builder.build_store(ptr, void_check(value)?)?;
                // }
                for statement in body {
                    self.build_statement(statement)?;
                }
                Ok(())
            }
            Return(expression) => {
                if let Some(expression) = expression {
                    let ret_value = self.build_expression(expression)?;
                    self.builder.build_return(Some(&ret_value))?;
                } else {
                    self.builder.build_return(None)?;
                }
                Ok(())
            }
            Expression(expression) => {
                self.build_expression(expression)?;
                Ok(())
            }
            VoidFunctionCall(function_declaration, vec) => todo!(),
        }
    }

    fn build_expression<'ctx>(
        &self,
        expr: semantic::expression::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        todo!()
    }
}

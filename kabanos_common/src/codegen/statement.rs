use inkwell::{builder::Builder, context::Context, values::FunctionValue};

use crate::semantic;

use super::{
    error::CodegenResult,
    symbol_table::{Symbol, SymbolTable},
};

impl semantic::Statement {
    pub fn build_statement<'ctx>(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        function: FunctionValue,
        symbol_table: &mut SymbolTable<'ctx>,
    ) -> CodegenResult {
        match self {
            Self::LocalVar(ref name, ref datatype, ref value) => {
                let ty = datatype.to_llvm_type(context);

                let ptr = builder.build_alloca(ty, &name)?;
                if let Some(expression) = value {
                    let value = expression.build_expression(context, builder, symbol_table)?;
                    builder.build_store(ptr, void_check(value)?)?;
                }

                let symbol = Symbol { ptr, ty };

                symbol_table.push_value(name, symbol);
                Ok(())
            }
            Self::Conditional(condition, block, else_block_) => {
                let condition =
                    void_check(condition.build_expression(context, builder, symbol_table)?)?;

                let then_block = context.append_basic_block(function, "then");
                let else_block = context.append_basic_block(function, "else");
                let merge_block = context.append_basic_block(function, "merge");

                builder.build_conditional_branch(
                    condition.into_int_value(), // lol
                    then_block,
                    else_block,
                )?;

                builder.position_at_end(then_block);
                block.build_statement(context, builder, function, symbol_table)?;
                builder.build_unconditional_branch(merge_block)?;

                builder.position_at_end(else_block);
                if let Some(else_block_) = else_block_ {
                    else_block_.build_statement(context, builder, function, symbol_table)?;
                }
                builder.build_unconditional_branch(merge_block)?;

                builder.position_at_end(merge_block);
                Ok(())
            }
            Self::Loop(condition, body) => {
                let loop_block = context.append_basic_block(function, "loop");
                let body_block = context.append_basic_block(function, "body");
                let continue_block = context.append_basic_block(function, "continue");

                builder.build_unconditional_branch(loop_block)?;
                builder.position_at_end(loop_block);

                let condition =
                    void_check(condition.build_expression(context, builder, symbol_table)?)?;

                builder.build_conditional_branch(
                    condition.into_int_value(), // lol
                    body_block,
                    continue_block,
                )?;

                builder.position_at_end(body_block);
                body.build_statement(context, builder, function, symbol_table)?;
                builder.build_unconditional_branch(loop_block)?;

                builder.position_at_end(continue_block);
                Ok(())
            }
            Self::Block(statements) => {
                symbol_table.push_scope();
                for statement in statements {
                    statement.build_statement(context, builder, function, symbol_table)?;
                }
                symbol_table.pop_scope();
                Ok(())
            }
            Self::Return(expression) => {
                if let Some(expression) = expression {
                    let ret_value = expression.build_expression(context, builder, symbol_table)?;
                    builder.build_return(Some(&void_check(ret_value)?))?;
                } else {
                    builder.build_return(None)?;
                }
                Ok(())
            }
            Self::Expression(expression) => {
                expression.build_expression(context, builder, symbol_table)?;
                Ok(())
            }
        }
    }
}

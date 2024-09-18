use core::panic;

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::BasicValue,
};

use crate::ast;

pub struct IRContext<'ctx, 'a> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

#[derive(Debug)]
pub enum IRBuilerError {
    LLVMBuilderError(BuilderError),
}

impl From<BuilderError> for IRBuilerError {
    fn from(value: BuilderError) -> Self {
        Self::LLVMBuilderError(value)
    }
}

pub type CodegenResult = std::result::Result<(), IRBuilerError>;

pub trait IRBuilder {
    fn codegen(&self, ctx: &IRContext) -> CodegenResult;
}

// impl IRBuilder<'a, 'b> for Vec<ast::FunctionDefinition> {
//     fn codegen<'a, 'b> (&self, context: &'a IRContext<'b>) -> Result {
//         for function in self {
//             function.codegen(context)?
//         }
//         Ok(())
//     }
// }

fn str_to_basictype<'a>(ctx: &'a Context, s: &str) -> BasicTypeEnum<'a> {
    match s {
        "bool" => ctx.bool_type().into(),
        "i8" => ctx.i8_type().into(),
        "i16" => ctx.i16_type().into(),
        "i32" => ctx.i32_type().into(),
        "i64" => ctx.i64_type().into(),
        "f32" => ctx.f32_type().into(),
        "f64" => ctx.f64_type().into(),
        _ => panic!("Unknown type {}", s),
    }
}

impl IRBuilder for ast::FunctionDefinition {
    fn codegen(&self, ctx: &IRContext) -> CodegenResult {
        let params: Vec<BasicMetadataTypeEnum> = self
            .parameters
            .iter()
            .map(|p| str_to_basictype(ctx.context, &p.param_type).into())
            .collect();

        let fn_type = match &self.return_type {
            Some(t) => {
                let return_type = str_to_basictype(ctx.context, t);
                return_type.fn_type(&params, false)
            }
            None => ctx.context.void_type().fn_type(&params, false),
        };

        let function = ctx.module.add_function(&self.name, fn_type, None);
        for (i, p) in self.parameters.iter().enumerate() {
            let param = function.get_nth_param(i as u32).unwrap();
            param.set_name(&p.name);
        }

        let block = ctx.context.append_basic_block(function, "entry");
        ctx.builder.position_at_end(block);
        for statement in &self.body {
            statement.codegen(ctx)?;
        }
        // ctx.builder.build_return(Some(&param))?;
        Ok(())
    }
}

impl IRBuilder for ast::Statement {
    fn codegen(&self, ctx: &IRContext) -> CodegenResult {
        match self {
            &Self::LocalVar(ref name, ref datatype, ref value) => {
                let datatype = str_to_basictype(ctx.context, datatype.as_deref().unwrap_or("i32"));
                let local_var_ptr = ctx.builder.build_alloca(datatype, &name)?;
                if let Some(expression) = value {
                    // TODO: const boohoo
                    let value = datatype.const_zero();
                    ctx.builder.build_store(local_var_ptr, value)?;
                }
                Ok(())
            }
            &Self::Loop(condition, block) => {
                let i32_type = ctx.context.i32_type();
                let condition_value = i32_type.const_int(10, false); // Constant value for comparison
                let zero_value = i32_type.const_int(0, false);
                let condition = ctx.builder.build_int_compare(
                    inkwell::IntPredicate::SGT,
                    condition_value,
                    zero_value,
                    "if_cond",
                );

                // Step 6: Create the blocks for "then", "else", and "merge"
                let then_block = ctx.context.append_basic_block(function, "then");
                let else_block = ctx.context.append_basic_block(function, "else");
                let merge_block = ctx.context.append_basic_block(function, "merge");

                // Step 7: Build the conditional branch
                builder.build_conditional_branch(condition, then_block, else_block);

                // Step 8: Fill in the "then" block (assign 42 to the variable)
                builder.position_at_end(then_block);
                let then_value = i32_type.const_int(42, false);
                builder.build_store(local_var_ptr, then_value);
                builder.build_unconditional_branch(merge_block); // Jump to merge block

                // Step 9: Fill in the "else" block (assign 0 to the variable)
                builder.position_at_end(else_block);
                let else_value = i32_type.const_int(0, false);
                builder.build_store(local_var_ptr, else_value);
                builder.build_unconditional_branch(merge_block); // Jump to merge block

                // Step 10: Continue at the merge block
                builder.position_at_end(merge_block);
                let loaded_var = builder.build_load(local_var_ptr, "loaded_var");
                Ok(())
            }
            _ => Ok(println!("Unhandled statement")),
        }
    }
}

// impl IRBuilder for ast::Expression {
//     fn codegen(&self, context: &Context, builder: &Builder, module: &Module) -> Result {
//         // match self {
//         //     &Self::IntegerLiteral(integer) => {
//         //         let i64_type = context.i64_type();
//         //     }
//         // }
//         Ok(())
//     }
// }

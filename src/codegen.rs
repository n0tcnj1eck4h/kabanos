use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
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

fn str_to_anytype<'a>(ctx: &'a Context, s: &str) -> Result<BasicTypeEnum<'a>, ()> {
    match s {
        "bool" => Ok(ctx.bool_type().into()),
        "i8" => Ok(ctx.i8_type().into()),
        "i16" => Ok(ctx.i16_type().into()),
        "i32" => Ok(ctx.i32_type().into()),
        "i64" => Ok(ctx.i64_type().into()),
        "f32" => Ok(ctx.f32_type().into()),
        "f64" => Ok(ctx.f64_type().into()),
        _ => Err(()),
    }
}

impl IRBuilder for ast::FunctionDefinition {
    fn codegen(&self, ctx: &IRContext) -> CodegenResult {
        let return_type = self
            .return_type
            .map(|s| str_to_anytype(ctx.context, &s).unwrap())
            .unwrap_or(ctx.context.void_type().into());

        let fn_type = return_type.fn_type(params, false);

        let function = ctx.module.add_function(&self.name, fn_type, None);
        let param = function.get_first_param().unwrap().into_int_value();
        // param.set_name("x");
        let block = ctx.context.append_basic_block(function, "entry");
        ctx.builder.position_at_end(block);
        ctx.builder.build_return(Some(&param))?;
        ctx.module.print_to_stderr();
        Ok(())
    }
}

// impl IRBuilder for ast::Statement {
//     fn codegen(&self, context: &IRContext) -> Result {
//         match self {
//             // &Self::Expression(ref expr) => expr.codegen(context, builder, module),
//             _ => Err(()),
//         }
//     }
// }

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

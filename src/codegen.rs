use core::panic;
use std::collections::{HashMap, VecDeque};

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    IntPredicate,
};

use crate::{ast, token::Operator};

#[derive(Debug)]
pub enum IRBuilerError {
    LLVMBuilderError(BuilderError),
}

impl From<BuilderError> for IRBuilerError {
    fn from(value: BuilderError) -> Self {
        Self::LLVMBuilderError(value)
    }
}

pub type CodegenResult<T = ()> = std::result::Result<T, IRBuilerError>;

pub trait ModuleProvider {
    fn build_module<'a>(&self, context: &'a Context, name: &str) -> CodegenResult<Module<'a>>;
}

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
impl ModuleProvider for ast::Module {
    fn build_module<'a>(&self, context: &'a Context, name: &str) -> CodegenResult<Module<'a>> {
        let builder = context.create_builder();
        let module = context.create_module(name);
        let mut symbol_table = SymbolTable::default();

        for fn_def in &self.function_definitions {
            symbol_table.push_scope();

            let mut params: Vec<BasicMetadataTypeEnum> = Vec::new();
            for param in fn_def.parameters.iter() {
                params.push(str_to_basictype(context, &param.param_type).into());
            }

            let fn_type = match &fn_def.return_type {
                Some(t) => {
                    let return_type = str_to_basictype(context, &t);
                    return_type.fn_type(&params, false)
                }
                None => context.void_type().fn_type(&params, false),
            };

            let function = module.add_function(&fn_def.name, fn_type, None);
            for (i, p) in fn_def.parameters.iter().enumerate() {
                let param = function.get_nth_param(i as u32).unwrap();
                param.set_name(&p.name);
                symbol_table.push_value(&p.name, param.into());
            }

            let block = context.append_basic_block(function, "");
            builder.position_at_end(block);
            for statement in &fn_def.body {
                statement.build_ir(context, &builder, &module, function, &mut symbol_table)?;
            }

            symbol_table.pop_scope();
        }

        Ok(module)
    }
}

#[derive(Default)]
pub struct SymbolTable<'ctx> {
    scope_stack: VecDeque<HashMap<String, BasicValueEnum<'ctx>>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn push_value(&mut self, name: &str, value: BasicValueEnum<'ctx>) {
        self.scope_stack
            .back_mut()
            .expect("There is no stack to put local var in")
            .insert(name.into(), value);
    }

    pub fn push_scope(&mut self) {
        self.scope_stack.push_back(Default::default());
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop_back();
    }

    pub fn get_value(&self, name: &str) -> Option<BasicValueEnum<'ctx>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(*value);
            }
        }

        // if let Some(value) = self.globals.get(name) {
        //     return Some(*value);
        // }
        None
    }
}

pub trait StatementBuilder {
    fn build_ir<'ctx>(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        module: &Module<'ctx>,
        function: FunctionValue,
        symbol_table: &mut SymbolTable,
    ) -> CodegenResult;
}

impl StatementBuilder for ast::Statement {
    fn build_ir<'ctx>(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        module: &Module<'ctx>,
        function: FunctionValue,
        symbol_table: &mut SymbolTable,
    ) -> CodegenResult {
        match &self {
            &Self::LocalVar(ref name, ref datatype, ref value) => {
                let datatype = str_to_basictype(context, datatype.as_deref().unwrap_or("i32"));
                let local_var_ptr = builder.build_alloca(datatype, &name)?;
                if let Some(expression) = value {
                    let value = expression.build_expression(context, builder, symbol_table)?;
                    builder.build_store(local_var_ptr, value)?;
                }
                Ok(())
            }
            &Self::Conditional(condition, block, else_block_) => {
                let condition = condition.build_expression(context, builder, symbol_table)?;

                let then_block = context.append_basic_block(function, "");
                let else_block = context.append_basic_block(function, "");
                let merge_block = context.append_basic_block(function, "");

                builder.build_conditional_branch(
                    condition.into_int_value(), // lol
                    then_block,
                    else_block,
                )?;

                builder.position_at_end(then_block);
                block.build_ir(context, builder, module, function, symbol_table)?;
                builder.build_unconditional_branch(merge_block)?;

                builder.position_at_end(else_block);
                if let Some(else_block_) = else_block_ {
                    else_block_.build_ir(context, builder, module, function, symbol_table)?;
                }
                builder.build_unconditional_branch(merge_block)?;

                builder.position_at_end(merge_block);
                Ok(())
            }
            _ => Ok(println!("Unhandled statement")),
        }
    }
}

pub trait ExpressionBuilder {
    fn build_expression<'ctx>(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        symbol_table: &SymbolTable<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>>;
}

impl ExpressionBuilder for ast::Expression {
    fn build_expression<'ctx>(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        symbol_table: &SymbolTable<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match self {
            Self::Identifier(ident) => Ok(symbol_table
                .get_value(ident)
                .expect("Identifier not on stack")
                .into()),
            Self::BooleanLiteral(b) => Ok(context.bool_type().const_int(*b as u64, false).into()),
            Self::IntegerLiteral(int) => {
                Ok(context.i32_type().const_int(*int as u64, false).into())
            }
            Self::FloatingPointLiteral(f) => Ok(context.f32_type().const_float(*f).into()),
            Self::BinaryOperation(l, op, r) => {
                let l = l.build_expression(context, builder, symbol_table)?;
                let r = r.build_expression(context, builder, symbol_table)?;
                if let (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) = (l, r) {
                    return match *op {
                        Operator::Add => Ok(builder.build_int_add(l, r, "")?.into()),
                        Operator::Subtract => Ok(builder.build_int_sub(l, r, "")?.into()),
                        Operator::Multiply => Ok(builder.build_int_mul(l, r, "")?.into()),
                        Operator::Divide => Ok(builder.build_int_signed_div(l, r, "")?.into()),
                        Operator::Equal => Ok(builder
                            .build_int_compare(IntPredicate::EQ, l, r, "eq")?
                            .into()),
                        Operator::NotEqual => Ok(builder
                            .build_int_compare(IntPredicate::NE, l, r, "")?
                            .into()),
                        Operator::Greater => Ok(builder
                            .build_int_compare(IntPredicate::SGT, l, r, "")?
                            .into()),
                        Operator::GreaterOrEqual => Ok(builder
                            .build_int_compare(IntPredicate::SGE, l, r, "")?
                            .into()),
                        Operator::Less => Ok(builder
                            .build_int_compare(IntPredicate::SLT, l, r, "")?
                            .into()),
                        Operator::LessOrEqual => Ok(builder
                            .build_int_compare(IntPredicate::SLE, l, r, "")?
                            .into()),
                        _ => panic!("Unhandled binary operator {:?}", op),
                    };
                }
                panic!(
                    "Binary operation between {:?} and {:?} is not yet implemented",
                    l, r
                );
            }
            Self::UnaryOperation(op, expr) => {
                // TODO
                Ok(expr.build_expression(context, builder, symbol_table)?)
            }
            Self::FunctionCall(_, _) => panic!("function calls are not supported yet"),
            Self::StringLiteral(_) => panic!("string literals are not supported yet"),
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

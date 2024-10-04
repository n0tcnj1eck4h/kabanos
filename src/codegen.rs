use core::panic;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
    IntPredicate,
};

use crate::semantic::{self, operator::BinaryOperator, primitive::Primitive, LValue};

#[derive(Debug)]
pub enum IRBuilerError {
    LLVMBuilderError(BuilderError),
}

impl Display for IRBuilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LLVMBuilderError(err) => write!(f, "{:?}", err),
        }
    }
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

impl Primitive {
    pub fn to_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            Primitive::Bool => context.bool_type().into(),
            Primitive::I8 => context.i8_type().into(),
            Primitive::I16 => context.i16_type().into(),
            Primitive::I32 => context.i32_type().into(),
            Primitive::I64 => context.i64_type().into(),
            Primitive::U8 => context.i8_type().into(), // ermmm llvm?
            Primitive::U16 => context.i16_type().into(),
            Primitive::U32 => context.i32_type().into(),
            Primitive::U64 => context.i64_type().into(),
            Primitive::F32 => context.f32_type().into(),
            Primitive::F64 => context.f64_type().into(),
        }
    }
}

impl ModuleProvider for semantic::Module {
    fn build_module<'a>(&self, context: &'a Context, name: &str) -> CodegenResult<Module<'a>> {
        let builder = context.create_builder();
        let module = context.create_module(name);
        let mut symbol_table = SymbolTable::default();

        for fn_def in &self.functions {
            symbol_table.push_scope();

            let mut params = Vec::new();
            for param in fn_def.params.iter() {
                params.push(param.ty.to_llvm_type(context).into());
            }

            let fn_type = match &fn_def.ty {
                Some(t) => {
                    let return_type = t.to_llvm_type(context);
                    return_type.fn_type(&params, false)
                }
                None => context.void_type().fn_type(&params, false),
            };

            let function = module.add_function(&fn_def.name, fn_type, None);

            let block = context.append_basic_block(function, "");
            builder.position_at_end(block);

            for (i, p) in fn_def.params.iter().enumerate() {
                let param = function.get_nth_param(i as u32).unwrap();
                param.set_name(&p.name);

                let param_ptr = builder.build_alloca(param.get_type(), "param_ptr")?;
                builder.build_store(param_ptr, param)?;

                let symbol = Symbol {
                    ptr: param_ptr,
                    ty: param.get_type(),
                };

                symbol_table.push_value(&p.name, symbol);
            }

            for statement in &fn_def.body {
                statement.build_statement(context, &builder, function, &mut symbol_table)?;
            }

            symbol_table.pop_scope();
        }

        Ok(module)
    }
}

#[derive(Copy, Clone)]
pub struct Symbol<'ctx> {
    ptr: PointerValue<'ctx>,
    ty: BasicTypeEnum<'ctx>,
}

#[derive(Default)]
pub struct SymbolTable<'ctx> {
    scope_stack: VecDeque<HashMap<String, Symbol<'ctx>>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn push_value(&mut self, name: &str, symbol: Symbol<'ctx>) {
        self.scope_stack
            .back_mut()
            .expect("There is no stack to put local var in")
            .insert(name.into(), symbol);
    }

    pub fn push_scope(&mut self) {
        self.scope_stack.push_back(Default::default());
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop_back();
    }

    pub fn get_value(&self, name: &str) -> Option<Symbol<'ctx>> {
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
    fn build_statement<'ctx>(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        function: FunctionValue,
        symbol_table: &mut SymbolTable<'ctx>,
    ) -> CodegenResult;
}

impl StatementBuilder for semantic::Statement {
    fn build_statement<'ctx>(
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
                    builder.build_store(ptr, value)?;
                }

                let symbol = Symbol { ptr, ty };

                symbol_table.push_value(name, symbol);
                Ok(())
            }
            Self::Conditional(condition, block, else_block_) => {
                let condition = condition.build_expression(context, builder, symbol_table)?;

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
                let condition = condition.build_expression(context, builder, symbol_table)?;
                let loop_block = context.append_basic_block(function, "loop");
                let merge_block = context.append_basic_block(function, "continue");

                builder.position_at_end(loop_block);
                builder.build_conditional_branch(
                    condition.into_int_value(), // lol
                    loop_block,
                    merge_block,
                )?;

                body.build_statement(context, builder, function, symbol_table)?;

                builder.build_unconditional_branch(loop_block)?;
                builder.position_at_end(merge_block);
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
                let ret_value = expression.build_expression(context, builder, symbol_table)?;
                builder.build_return(Some(&ret_value))?;
                Ok(())
            }
            Self::Expression(expression) => {
                expression.build_expression(context, builder, symbol_table)?;
                Ok(())
            }
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

impl ExpressionBuilder for semantic::Expression {
    fn build_expression<'ctx>(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        symbol_table: &SymbolTable<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match self {
            Self::Assignment(LValue::Identifier(ident), expr) => {
                let r = expr.build_expression(context, builder, symbol_table)?;
                let symbol = symbol_table.get_value(&ident).expect("lval is undefined");
                if symbol.ty != r.get_type() {
                    panic!("assignemtn type mismatch")
                }
                builder.build_store(symbol.ptr, r)?;
                return Ok(builder.build_load(symbol.ty, symbol.ptr, ident)?);
            }
            Self::Assignment(..) => unimplemented!(),
            Self::LValue(semantic::LValue::Identifier(identifier)) => {
                let symbol = symbol_table
                    .get_value(identifier)
                    .expect(&format!("Identifier {} not on stack", identifier));
                Ok(builder.build_load(symbol.ty, symbol.ptr, &identifier)?)
            }
            Self::LValue(_) => unimplemented!(),
            Self::BooleanLiteral(b) => Ok(context.bool_type().const_int(*b as u64, false).into()),
            Self::IntegerLiteral(int) => {
                Ok(context.i32_type().const_int(*int as u64, false).into())
            }
            Self::FloatLiteral(f) => Ok(context.f32_type().const_float(*f).into()),
            Self::BinaryOperation(lexpr, op, rexpr) => {
                let mut l = lexpr.build_expression(context, builder, symbol_table)?;
                let r = rexpr.build_expression(context, builder, symbol_table)?;

                if let (BasicValueEnum::IntValue(l_), BasicValueEnum::FloatValue(r)) = (l, r) {
                    l = builder
                        .build_signed_int_to_float(l_, r.get_type(), "fcast")?
                        .into();
                }

                if let (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) = (l, r) {
                    return Ok(build_int_binop(builder, *op, l, r)?.into());
                }

                panic!(
                    "Binary operation between {:?} and {:?} is not yet implemented",
                    l, r
                );
            }
            Self::UnaryOperation(_op, expr) => {
                // TODO
                Ok(expr.build_expression(context, builder, symbol_table)?)
            }
            Self::FunctionCall(_, _) => panic!("function calls are not supported yet"),
            // Self::StringLiteral(_) => panic!("string literals are not supported yet"),
        }
    }
}

fn build_int_binop<'ctx>(
    builder: &Builder<'ctx>,
    op: BinaryOperator,
    l: IntValue<'ctx>,
    r: IntValue<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    return match op {
        BinaryOperator::Add => Ok(builder.build_int_add(l, r, "add")?),
        BinaryOperator::Subtract => Ok(builder.build_int_sub(l, r, "sub")?),
        BinaryOperator::Multiply => Ok(builder.build_int_mul(l, r, "mul")?),
        BinaryOperator::Divide => Ok(builder.build_int_signed_div(l, r, "div")?),
        BinaryOperator::Equal => Ok(builder.build_int_compare(IntPredicate::EQ, l, r, "eq")?),
        BinaryOperator::NotEqual => Ok(builder.build_int_compare(IntPredicate::NE, l, r, "neq")?),
        BinaryOperator::Greater => Ok(builder.build_int_compare(IntPredicate::SGT, l, r, "gt")?),
        BinaryOperator::Less => Ok(builder.build_int_compare(IntPredicate::SLT, l, r, "slt")?),
        BinaryOperator::Modulo => Ok(builder.build_int_signed_rem(l, r, "srem")?),
        BinaryOperator::BitAnd => Ok(builder.build_and(l, r, "and")?),
        BinaryOperator::BitOr => Ok(builder.build_or(l, r, "or")?),
        BinaryOperator::BitXor => Ok(builder.build_xor(l, r, "xor")?),
        BinaryOperator::BitLeft => Ok(builder.build_left_shift(l, r, "lshift")?),
        BinaryOperator::BitRight => Ok(builder.build_right_shift(l, r, false, "rshift")?),
        BinaryOperator::LogicAnd => Ok(builder.build_and(l, r, "and")?),
        BinaryOperator::LogicOr => Ok(builder.build_or(l, r, "or")?),
        BinaryOperator::GreaterOrEqual => {
            Ok(builder.build_int_compare(IntPredicate::SGE, l, r, "ge")?)
        }
        BinaryOperator::LessOrEqual => {
            Ok(builder.build_int_compare(IntPredicate::SLE, l, r, "le")?)
        }
    };
}

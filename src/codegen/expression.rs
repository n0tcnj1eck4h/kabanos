use inkwell::{
    builder::Builder,
    context::Context,
    values::{BasicValueEnum, IntValue},
    IntPredicate,
};

use crate::semantic::{self, BinaryOperator, LValue};

use super::{error::CodegenResult, symbol_table::SymbolTable};

impl semantic::Expression {
    pub fn build_expression<'ctx>(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        symbol_table: &SymbolTable<'ctx>,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        match self {
            Self::Assignment(LValue::Identifier(ident), expr) => {
                let r = void_check(expr.build_expression(context, builder, symbol_table)?)?;
                let symbol = symbol_table.get_value(&ident).expect("lval is undefined");
                if symbol.ty != r.get_type() {
                    panic!("type mismatch");
                }
                builder.build_store(symbol.ptr, r)?;
                return Ok(Some(builder.build_load(symbol.ty, symbol.ptr, ident)?));
            }
            Self::LValue(LValue::Identifier(identifier)) => {
                let symbol = symbol_table
                    .get_value(identifier)
                    .expect(&format!("Identifier {} not on stack", identifier));
                Ok(Some(builder.build_load(
                    symbol.ty,
                    symbol.ptr,
                    &identifier,
                )?))
            }
            Self::BooleanLiteral(b) => {
                Ok(Some(context.bool_type().const_int(*b as u64, false).into()))
            }
            Self::IntegerLiteral(int) => Ok(Some(
                context.i32_type().const_int(*int as u64, false).into(),
            )),
            Self::FloatLiteral(f) => Ok(Some(context.f32_type().const_float(*f).into())),
            Self::BinaryOperation(lexpr, op, rexpr) => {
                let mut l = void_check(lexpr.build_expression(context, builder, symbol_table)?)?;
                let r = void_check(rexpr.build_expression(context, builder, symbol_table)?)?;

                if let (BasicValueEnum::IntValue(l_), BasicValueEnum::FloatValue(r)) = (l, r) {
                    l = builder
                        .build_signed_int_to_float(l_, r.get_type(), "fcast")?
                        .into();
                }

                if let (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) = (l, r) {
                    return Ok(Some(build_int_binop(builder, *op, l, r)?.into()));
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
            Self::FunctionCall(name, arguments) => {
                let fn_value = symbol_table
                    .get_function(name)
                    .expect("undeclared function");
                let mut args = Vec::new();
                for a in arguments {
                    let a = void_check(a.build_expression(context, builder, symbol_table)?)?;
                    args.push(a.into());
                }
                let call_site = builder.build_call(fn_value, &args, name)?;
                if let Some(ret_val) = call_site.try_as_basic_value().left() {
                    Ok(Some(ret_val))
                } else {
                    Ok(None)
                }
            } // Self::StringLiteral(_) => panic!("string literals are not supported yet"),
        }
    }
}

fn build_int_binop<'ctx>(
    builder: &Builder<'ctx>,
    op: BinaryOperator,
    l: IntValue<'ctx>,
    r: IntValue<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    use BinaryOperator::*;
    return match op {
        Add => Ok(builder.build_int_add(l, r, "add")?),
        Subtract => Ok(builder.build_int_sub(l, r, "sub")?),
        Multiply => Ok(builder.build_int_mul(l, r, "mul")?),
        Divide => Ok(builder.build_int_signed_div(l, r, "div")?),
        Equal => Ok(builder.build_int_compare(IntPredicate::EQ, l, r, "eq")?),
        NotEqual => Ok(builder.build_int_compare(IntPredicate::NE, l, r, "neq")?),
        Greater => Ok(builder.build_int_compare(IntPredicate::SGT, l, r, "gt")?),
        Less => Ok(builder.build_int_compare(IntPredicate::SLT, l, r, "slt")?),
        Modulo => Ok(builder.build_int_signed_rem(l, r, "srem")?),
        BitAnd => Ok(builder.build_and(l, r, "and")?),
        BitOr => Ok(builder.build_or(l, r, "or")?),
        BitXor => Ok(builder.build_xor(l, r, "xor")?),
        BitLeft => Ok(builder.build_left_shift(l, r, "lshift")?),
        BitRight => Ok(builder.build_right_shift(l, r, false, "rshift")?),
        LogicAnd => Ok(builder.build_and(l, r, "and")?),
        LogicOr => Ok(builder.build_or(l, r, "or")?),
        GreaterOrEqual => Ok(builder.build_int_compare(IntPredicate::SGE, l, r, "ge")?),
        LessOrEqual => Ok(builder.build_int_compare(IntPredicate::SLE, l, r, "le")?),
    };
}

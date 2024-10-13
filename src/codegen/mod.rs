mod error;
mod expression;
mod module;
mod statement;
mod symbol_table;

use crate::semantic::Primitive;
use inkwell::{context::Context, types::BasicTypeEnum};

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

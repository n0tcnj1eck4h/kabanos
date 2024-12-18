mod error;
mod expression;
mod module;
mod statement;

use crate::semantic::types::{FloatTy, IntBitWidth, IntegerTy, TypeKind};
use inkwell::{context::Context, types::BasicTypeEnum};

impl TypeKind {
    pub fn to_llvm_type<'ctx>(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use IntBitWidth::*;
        match self {
            TypeKind::Boolean => ctx.bool_type().into(),
            TypeKind::FloatType(FloatTy::F32) => ctx.f32_type().into(),
            TypeKind::FloatType(FloatTy::F64) => ctx.f64_type().into(),
            TypeKind::IntType(IntegerTy { bits: I8, .. }) => ctx.i8_type().into(),
            TypeKind::IntType(IntegerTy { bits: I16, .. }) => ctx.i16_type().into(),
            TypeKind::IntType(IntegerTy { bits: I32, .. }) => ctx.i32_type().into(),
            TypeKind::IntType(IntegerTy { bits: I64, .. }) => ctx.i64_type().into(),
        }
    }
}

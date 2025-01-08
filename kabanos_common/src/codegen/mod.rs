pub mod error;
pub mod module;
pub mod statement;

use crate::semantic::types::{FloatTy, IntSizes, IntTy, Type};
use inkwell::{context::Context, types::BasicTypeEnum, AddressSpace};

impl Type {
    pub fn to_llvm_type<'ctx>(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use IntSizes::*;
        match self {
            Type::Bool => ctx.bool_type().into(),
            Type::Float(FloatTy::F32) => ctx.f32_type().into(),
            Type::Float(FloatTy::F64) => ctx.f64_type().into(),
            Type::Int(IntTy { bits: I8, .. }) => ctx.i8_type().into(),
            Type::Int(IntTy { bits: I16, .. }) => ctx.i16_type().into(),
            Type::Int(IntTy { bits: I32, .. }) => ctx.i32_type().into(),
            Type::Int(IntTy { bits: I64, .. }) => ctx.i64_type().into(),
            Type::Ptr(_) => ctx.ptr_type(AddressSpace::default()).into(),
        }
    }
}

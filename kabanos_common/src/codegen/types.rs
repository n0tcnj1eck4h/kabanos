use inkwell::{context::Context, types::BasicTypeEnum, AddressSpace};

use crate::semantic::types::{FloatTy, IntTy, Type, TypeKind};

pub enum DecayedType {
    Float(FloatTy),
    Int(IntTy),
    Ptr,
}

impl From<Type> for DecayedType {
    fn from(value: Type) -> Self {
        if value.is_ptr() {
            return DecayedType::Ptr;
        }
        match value.kind {
            TypeKind::Int(int_ty) => DecayedType::Int(int_ty),
            TypeKind::Float(float_ty) => DecayedType::Float(float_ty),
            TypeKind::Bool => DecayedType::Int(IntTy::U8),
        }
    }
}

impl DecayedType {
    pub fn to_llvm_type<'ctx>(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use IntTy::*;
        match self {
            DecayedType::Float(FloatTy::F32) => ctx.f32_type().into(),
            DecayedType::Float(FloatTy::F64) => ctx.f64_type().into(),
            DecayedType::Int(I8) | DecayedType::Int(U8) => ctx.i8_type().into(),
            DecayedType::Int(I16) | DecayedType::Int(U16) => ctx.i16_type().into(),
            DecayedType::Int(I32) | DecayedType::Int(U32) => ctx.i32_type().into(),
            DecayedType::Int(I64) | DecayedType::Int(U64) => ctx.i64_type().into(),
            DecayedType::Ptr => ctx.ptr_type(AddressSpace::default()).into(),
        }
    }
}

use std::fmt::{Debug, Display};

use super::primitive::Primitive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntBitWidth {
    I8 = 8,
    I16 = 16,
    I32 = 32,
    I64 = 64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntegerTy {
    pub bits: IntBitWidth,
    pub sign: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatTy {
    F32 = 32,
    F64 = 64,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    IntType(IntegerTy),
    FloatType(FloatTy),
    Boolean,
}

impl From<Primitive> for TypeKind {
    #[rustfmt::skip]
    fn from(value: Primitive) -> Self {
        use IntBitWidth::*;
        use TypeKind::IntType;

        match value {
            Primitive::Bool => TypeKind::Boolean,
            Primitive::F32 => TypeKind::FloatType(FloatTy::F32),
            Primitive::F64 => TypeKind::FloatType(FloatTy::F64),
            Primitive::I8  => IntType(IntegerTy { bits: I8,  sign: true,  }),
            Primitive::U8  => IntType(IntegerTy { bits: I8,  sign: false, }),
            Primitive::I16 => IntType(IntegerTy { bits: I16, sign: true,  }),
            Primitive::U16 => IntType(IntegerTy { bits: I16, sign: false, }),
            Primitive::I32 => IntType(IntegerTy { bits: I32, sign: true,  }),
            Primitive::U32 => IntType(IntegerTy { bits: I32, sign: false, }),
            Primitive::I64 => IntType(IntegerTy { bits: I64, sign: true,  }),
            Primitive::U64 => IntType(IntegerTy { bits: I64, sign: false, }),
        }
    }
}

impl TypeKind {
    #[rustfmt::skip]
    pub fn as_str(&self) -> &'static str {
        use IntBitWidth::*;
        match self {
            TypeKind::Boolean => "bool",
            TypeKind::FloatType(FloatTy::F32) => "f32",
            TypeKind::FloatType(FloatTy::F64) => "f64",
            TypeKind::IntType(IntegerTy { bits: I8,  sign: true  }) => "i8",
            TypeKind::IntType(IntegerTy { bits: I8,  sign: false }) => "u8",
            TypeKind::IntType(IntegerTy { bits: I16, sign: true  }) => "i16",
            TypeKind::IntType(IntegerTy { bits: I16, sign: false }) => "u16",
            TypeKind::IntType(IntegerTy { bits: I32, sign: true  }) => "i32",
            TypeKind::IntType(IntegerTy { bits: I32, sign: false }) => "u32",
            TypeKind::IntType(IntegerTy { bits: I64, sign: true  }) => "i64",
            TypeKind::IntType(IntegerTy { bits: I64, sign: false }) => "u64",
        }
    }
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Debug for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

use super::primitive::Primitive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntBitWidth {
    I8 = 8,
    I16 = 16,
    I32 = 32,
    I64 = 64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntegerTy {
    pub bits: IntBitWidth,
    pub sign: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatTy {
    F32 = 32,
    F64 = 64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeKind {
    IntType(IntegerTy),
    FloatType(FloatTy),
}

impl From<Primitive> for TypeKind {
    fn from(value: Primitive) -> Self {
        use IntBitWidth::*;
        use TypeKind::IntType;

        match value {
            Primitive::Bool => IntType(IntegerTy {
                bits: I8,
                sign: false,
            }),
            Primitive::I8 => IntType(IntegerTy {
                bits: I8,
                sign: true,
            }),
            Primitive::I16 => IntType(IntegerTy {
                bits: I16,
                sign: true,
            }),
            Primitive::I32 => IntType(IntegerTy {
                bits: I32,
                sign: true,
            }),
            Primitive::I64 => IntType(IntegerTy {
                bits: I64,
                sign: true,
            }),
            Primitive::U8 => IntType(IntegerTy {
                bits: I8,
                sign: false,
            }),
            Primitive::U16 => IntType(IntegerTy {
                bits: I16,
                sign: false,
            }),
            Primitive::U32 => IntType(IntegerTy {
                bits: I32,
                sign: false,
            }),
            Primitive::U64 => IntType(IntegerTy {
                bits: I64,
                sign: false,
            }),
            Primitive::F32 => TypeKind::FloatType(FloatTy::F32),
            Primitive::F64 => TypeKind::FloatType(FloatTy::F64),
        }
    }
}

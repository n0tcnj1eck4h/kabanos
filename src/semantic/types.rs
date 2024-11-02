use super::primitive::Primitive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntBitWidths {
    I8 = 8,
    I16 = 16,
    I32 = 32,
    I64 = 64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntegerType {
    pub bit_width: IntBitWidths,
    pub signed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatType {
    F32 = 32,
    F64 = 64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeEnum {
    IntType(IntegerType),
    FloatType(FloatType),
}

impl From<Primitive> for TypeEnum {
    fn from(value: Primitive) -> Self {
        use IntBitWidths::*;
        use TypeEnum::IntType;

        match value {
            Primitive::Bool => IntType(IntegerType {
                bit_width: I8,
                signed: false,
            }),
            Primitive::I8 => IntType(IntegerType {
                bit_width: I8,
                signed: true,
            }),
            Primitive::I16 => IntType(IntegerType {
                bit_width: I16,
                signed: true,
            }),
            Primitive::I32 => IntType(IntegerType {
                bit_width: I32,
                signed: true,
            }),
            Primitive::I64 => IntType(IntegerType {
                bit_width: I64,
                signed: true,
            }),
            Primitive::U8 => IntType(IntegerType {
                bit_width: I8,
                signed: false,
            }),
            Primitive::U16 => IntType(IntegerType {
                bit_width: I16,
                signed: false,
            }),
            Primitive::U32 => IntType(IntegerType {
                bit_width: I32,
                signed: false,
            }),
            Primitive::U64 => IntType(IntegerType {
                bit_width: I64,
                signed: false,
            }),
            Primitive::F32 => TypeEnum::FloatType(FloatType::F32),
            Primitive::F64 => TypeEnum::FloatType(FloatType::F64),
        }
    }
}

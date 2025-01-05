use std::{
    fmt::{Debug, Display},
    str::FromStr,
};

use super::error::SemanticError;

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

// #[derive(Clone, Copy, PartialEq, Eq)]
// pub struct Type {
//     pointer_depth: u32,
//     kind: TypeKind,
// }

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    IntType(IntegerTy),
    FloatType(FloatTy),
    Boolean,
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

impl FromStr for TypeKind {
    type Err = SemanticError;
    #[rustfmt::skip]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use IntBitWidth::*;
        Ok(match s {
            "bool" => TypeKind::Boolean,                                  
            "f32"  => TypeKind::FloatType(FloatTy::F32),                  
            "f64"  => TypeKind::FloatType(FloatTy::F64),                  
            "i8"   => TypeKind::IntType(IntegerTy { bits: I8,  sign: true,  }),     
            "u8"   => TypeKind::IntType(IntegerTy { bits: I8,  sign: false, }),     
            "i16"  => TypeKind::IntType(IntegerTy { bits: I16, sign: true,  }),     
            "u16"  => TypeKind::IntType(IntegerTy { bits: I16, sign: false, }),     
            "i32"  => TypeKind::IntType(IntegerTy { bits: I32, sign: true,  }),     
            "u32"  => TypeKind::IntType(IntegerTy { bits: I32, sign: false, }),     
            "i64"  => TypeKind::IntType(IntegerTy { bits: I64, sign: true,  }),     
            "u64"  => TypeKind::IntType(IntegerTy { bits: I64, sign: false, }),     
            _ => return Err(SemanticError::NotPrimitive(s.to_string())),
        })
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

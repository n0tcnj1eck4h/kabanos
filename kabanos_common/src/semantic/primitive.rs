use std::str::FromStr;

use super::error::SemanticErrorKind;

#[derive(Debug, Clone, Copy)]
pub enum Primitive {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

impl FromStr for Primitive {
    type Err = SemanticErrorKind;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(Self::Bool),
            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            "f32" => Ok(Self::F32),
            "f64" => Ok(Self::F64),
            _ => Err(SemanticErrorKind::NotPrimitive(s.to_string())),
        }
    }
}

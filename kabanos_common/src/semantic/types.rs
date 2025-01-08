use std::fmt::{Debug, Display};

use crate::{
    ast,
    span::{HasSpan, Spanned, WithSpan},
};

use super::{
    error::SemanticError,
    expression::{Expression, LValue},
    operator::{BinaryOperator, UnaryOperator},
    symbol::SymbolTable,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntSizes {
    I8 = 8,
    I16 = 16,
    I32 = 32,
    I64 = 64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntTy {
    pub bits: IntSizes,
    pub sign: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatTy {
    F32 = 32,
    F64 = 64,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Ptr(Box<Type>),
    Int(IntTy),
    Float(FloatTy),
    Bool,
}

impl Type {
    #[rustfmt::skip]
    pub fn to_string(&self) -> String {
        use IntSizes::*;
        match self {
            Type::Bool => "bool",
            Type::Float(FloatTy::F32) => "f32",
            Type::Float(FloatTy::F64) => "f64",
            Type::Int(IntTy { bits: I8,  sign: true  }) => "i8",
            Type::Int(IntTy { bits: I8,  sign: false }) => "u8",
            Type::Int(IntTy { bits: I16, sign: true  }) => "i16",
            Type::Int(IntTy { bits: I16, sign: false }) => "u16",
            Type::Int(IntTy { bits: I32, sign: true  }) => "i32",
            Type::Int(IntTy { bits: I32, sign: false }) => "u32",
            Type::Int(IntTy { bits: I64, sign: true  }) => "i64",
            Type::Int(IntTy { bits: I64, sign: false }) => "u64",
            Type::Ptr(ty) => return format!("*{}", ty.to_string()),
        }.to_string()
    }
}

impl TryFrom<ast::Type> for Type {
    type Error = SemanticError;

    #[rustfmt::skip]
    fn try_from(value: ast::Type) -> Result<Self, Self::Error> {
        use IntSizes::*;
        let mut ty = match value.name.as_str() {
            "bool" => Type::Bool,                                  
            "f32"  => Type::Float(FloatTy::F32),                  
            "f64"  => Type::Float(FloatTy::F64),                  
            "i8"   => Type::Int(IntTy { bits: I8,  sign: true,  }),     
            "u8"   => Type::Int(IntTy { bits: I8,  sign: false, }),     
            "i16"  => Type::Int(IntTy { bits: I16, sign: true,  }),     
            "u16"  => Type::Int(IntTy { bits: I16, sign: false, }),     
            "i32"  => Type::Int(IntTy { bits: I32, sign: true,  }),     
            "u32"  => Type::Int(IntTy { bits: I32, sign: false, }),     
            "i64"  => Type::Int(IntTy { bits: I64, sign: true,  }),     
            "u64"  => Type::Int(IntTy { bits: I64, sign: false, }),     
            _ => return Err(SemanticError::NotPrimitive(value.name)),
        };

        for _ in 0..value.pointers {
            ty = Type::Ptr(Box::new(ty));
        }

        Ok(ty)
    }
}

impl TryFrom<Spanned<ast::Type>> for Type {
    type Error = Spanned<SemanticError>;

    #[rustfmt::skip]
    fn try_from(value: Spanned<ast::Type>) -> Result<Self, Self::Error> {
        let span = value.get_span();
        let value = value.unwrap();
        let ty: Type = value.try_into().map_err(|e: SemanticError| e.with_span(span))?;
        Ok(ty)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl SymbolTable {
    pub fn get_expression_type(&self, expr: &Expression) -> Type {
        match expr {
            Expression::Cast(_, ty) => ty.clone(),
            Expression::BooleanLiteral(_) => Type::Bool,
            Expression::IntegerLiteral(_, int_ty) => Type::Int(*int_ty),
            Expression::FloatLiteral(_, float_ty) => Type::Float(*float_ty),
            Expression::UnaryOperation(UnaryOperator::Deref, expr) => {
                let Type::Ptr(ty) = self.get_expression_type(&expr) else {
                    panic!("This should never happen");
                };
                return *ty;
            }
            Expression::UnaryOperation(UnaryOperator::Ref, expr) => {
                Type::Ptr(Box::new(self.get_expression_type(&expr)))
            }
            Expression::UnaryOperation(_, expr) => self.get_expression_type(&expr),
            Expression::FunctionCall(call) => {
                let fn_decl = self.get_function(call.id);
                fn_decl.ty.clone().expect("This should never happen")
            }
            Expression::Assignment(lvalue, _) | Expression::LValue(lvalue) => match lvalue {
                LValue::LocalVar(var_id) => {
                    let var = self.get_variable(*var_id);
                    var.ty.clone()
                }
            },
            Expression::BinaryOperation(expr, op, _) => match op {
                BinaryOperator::Comparaison(_) | BinaryOperator::Logic(_) => Type::Bool,
                BinaryOperator::Bitwise(_) | BinaryOperator::Arithmetic(_) => {
                    self.get_expression_type(expr)
                }
            },
        }
    }
}

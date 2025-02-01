use std::fmt::{Debug, Display};

use kabanos_common::{
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
pub enum IntTy {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
}

impl IntTy {
    pub fn bits(&self) -> u32 {
        match self {
            IntTy::I8 | IntTy::U8 => 8,
            IntTy::I16 | IntTy::U16 => 16,
            IntTy::I32 | IntTy::U32 => 32,
            IntTy::I64 | IntTy::U64 => 64,
        }
    }

    pub fn signed(&self) -> bool {
        match self {
            IntTy::I8 | IntTy::I16 | IntTy::I32 | IntTy::I64 => true,
            IntTy::U8 | IntTy::U16 | IntTy::U32 | IntTy::U64 => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatTy {
    F32 = 32,
    F64 = 64,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Type {
    pub ptr_depth: u16,
    pub kind: TypeKind,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Int(IntTy),
    Float(FloatTy),
    Bool,
}

impl From<IntTy> for TypeKind {
    fn from(value: IntTy) -> Self {
        Self::Int(value)
    }
}

impl From<FloatTy> for TypeKind {
    fn from(value: FloatTy) -> Self {
        Self::Float(value)
    }
}

impl Type {
    pub fn is_ptr(&self) -> bool {
        return self.ptr_depth > 0;
    }
}

impl<T: Into<TypeKind>> From<T> for Type {
    fn from(value: T) -> Self {
        let kind: TypeKind = value.into();
        Self { kind, ptr_depth: 0 }
    }
}

impl TryFrom<ast::Type> for Type {
    type Error = SemanticError;

    fn try_from(ast_ty: ast::Type) -> Result<Self, Self::Error> {
        let kind = match ast_ty.name.as_str() {
            "bool" => TypeKind::Bool,
            "f32" => TypeKind::Float(FloatTy::F32),
            "f64" => TypeKind::Float(FloatTy::F64),
            "i8" => TypeKind::Int(IntTy::I8),
            "u8" => TypeKind::Int(IntTy::U8),
            "i16" => TypeKind::Int(IntTy::I16),
            "u16" => TypeKind::Int(IntTy::U16),
            "i32" => TypeKind::Int(IntTy::I32),
            "u32" => TypeKind::Int(IntTy::U32),
            "i64" => TypeKind::Int(IntTy::I64),
            "u64" => TypeKind::Int(IntTy::U64),
            _ => return Err(SemanticError::NotPrimitive(ast_ty.name)),
        };

        Ok(Type {
            kind,
            ptr_depth: ast_ty.pointers,
        })
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
        use TypeKind::*;
        for _ in 0..self.ptr_depth {
            write!(f, "*")?;
        }
        let s = match self.kind {
            Bool => "bool",
            Float(FloatTy::F32) => "f32",
            Float(FloatTy::F64) => "f64",
            Int(IntTy::I8) => "i8",
            Int(IntTy::U8) => "u8",
            Int(IntTy::I16) => "i16",
            Int(IntTy::U16) => "u16",
            Int(IntTy::I32) => "i32",
            Int(IntTy::U32) => "u32",
            Int(IntTy::I64) => "i64",
            Int(IntTy::U64) => "u64",
        };
        write!(f, "{}", s)
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
            Expression::BooleanLiteral(_) => Type {
                kind: TypeKind::Bool,
                ptr_depth: 0,
            },
            Expression::IntegerLiteral(_, int_ty) => Type {
                kind: TypeKind::Int(*int_ty),
                ptr_depth: 0,
            },
            Expression::FloatLiteral(_, float_ty) => Type {
                kind: TypeKind::Float(*float_ty),
                ptr_depth: 0,
            },
            Expression::UnaryOperation(UnaryOperator::Deref, expr) => {
                let mut ty = self.get_expression_type(expr);
                ty.ptr_depth -= 1;
                ty
            }
            Expression::UnaryOperation(UnaryOperator::Ref, expr) => {
                let mut ty = self.get_expression_type(expr);
                ty.ptr_depth += 1;
                ty
            }
            Expression::UnaryOperation(_, expr) => self.get_expression_type(expr),
            Expression::FunctionCall(call) => {
                let fn_decl = self.get_function(call.id);
                fn_decl.ty.clone().expect("This should never happen")
            }
            Expression::LValue(lvalue) => match lvalue {
                LValue::LocalVar(var_id) => {
                    let var = self.get_variable(*var_id);
                    var.ty.clone()
                }
            },
            Expression::BinaryOperation(expr, op, _) => match op {
                BinaryOperator::Comparaison(_) | BinaryOperator::Logic(_) => Type {
                    kind: TypeKind::Bool,
                    ptr_depth: 0,
                },
                BinaryOperator::Bitwise(_) | BinaryOperator::Arithmetic(_) => {
                    self.get_expression_type(expr)
                }
            },
            Expression::StringLiteral(_) => Type {
                kind: TypeKind::Int(IntTy::I8),
                ptr_depth: 1,
            },
        }
    }
}

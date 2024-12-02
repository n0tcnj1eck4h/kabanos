use std::fmt::Display;

use inkwell::builder::BuilderError;

#[derive(Debug)]
pub enum IRBuilerError {
    LLVMBuilderError(BuilderError),
}

impl Display for IRBuilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LLVMBuilderError(err) => write!(f, "{:?}", err),
        }
    }
}

impl From<BuilderError> for IRBuilerError {
    fn from(value: BuilderError) -> Self {
        Self::LLVMBuilderError(value)
    }
}

pub type CodegenResult<T = ()> = std::result::Result<T, IRBuilerError>;

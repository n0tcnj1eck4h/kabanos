#[derive(Debug)]
pub struct ResponseError {
    code: ErrorCode,
    message: String,
    data: serde_json::Value,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorCode {
    // Defined by JSON-RPC
    ParseError = -32700,
    InvalidRequest = -32600,
    MethodNotFound = -32601,
    InvalidParams = -32602,
    InternalError = -32603,

    // JSON-RPC reserved error codes range
    JsonRpcReservedErrorRangeStart = -32099,
    ServerNotInitialized = -32002,
    UnknownErrorCode = -32001,
    ServerErrorEnd = -32000,

    RequestFailed = -32803,
    ServerCancelled = -32802,
    ContentModified = -32801,
    RequestCancelled = -32800,
}

impl ErrorCode {
    /// Converts an integer to an `ErrorCode`, if possible.
    pub fn from_code(code: i32) -> Option<Self> {
        match code {
            -32700 => Some(ErrorCode::ParseError),
            -32600 => Some(ErrorCode::InvalidRequest),
            -32601 => Some(ErrorCode::MethodNotFound),
            -32602 => Some(ErrorCode::InvalidParams),
            -32603 => Some(ErrorCode::InternalError),
            -32099 => Some(ErrorCode::JsonRpcReservedErrorRangeStart),
            -32002 => Some(ErrorCode::ServerNotInitialized),
            -32001 => Some(ErrorCode::UnknownErrorCode),
            -32803 => Some(ErrorCode::RequestFailed),
            -32802 => Some(ErrorCode::ServerCancelled),
            -32801 => Some(ErrorCode::ContentModified),
            -32800 => Some(ErrorCode::RequestCancelled),
            _ => None,
        }
    }
}

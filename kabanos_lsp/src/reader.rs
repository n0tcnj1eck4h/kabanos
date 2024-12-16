use std::{
    io::{self, BufRead},
    num::ParseIntError,
    str::Utf8Error,
};

use lsp_types::request::{Initialize, Request as _, Shutdown};
use serde_json::Value;
use thiserror::Error;

use crate::messages::{
    notification::Notification,
    request::{Request, RequestMethod},
    response::{Response, ResponseKind},
    Message,
};

pub struct MessageReader<R> {
    pub input: R,
    pub string_buffer: String,
    pub byte_buffer: Vec<u8>,
}

impl<R: BufRead> MessageReader<R> {
    pub fn new(input: R) -> Self {
        Self {
            input,
            string_buffer: String::with_capacity(512),
            byte_buffer: Vec::with_capacity(1024),
        }
    }

    fn next_impl(&mut self) -> Result<Message, MessageReaderError> {
        // Read headers
        let mut content_length = None;
        loop {
            self.string_buffer.clear();
            let n = self.input.read_line(&mut self.string_buffer)?;

            if n == 0 {
                return Err(MessageReaderError::EOF);
            }

            if self.string_buffer == "\r\n" {
                break;
            }

            let mut header = self.string_buffer.split(": ");
            let key = header.next().unwrap();
            let value = header.next().unwrap();

            match key {
                "Content-Length" => {
                    let length = value.trim().parse()?;
                    content_length = Some(length);
                }
                _ => {}
            }
        }

        let Some(content_length) = content_length else {
            return Err(MessageReaderError::MissingContentLenHeader);
        };

        // Parse body
        self.byte_buffer.resize(content_length, 0);
        self.input.read_exact(&mut self.byte_buffer)?;

        let json: Value = serde_json::from_slice(&self.byte_buffer)?;

        let rpc_version = json
            .get("jsonrpc")
            .ok_or(MessageReaderError::MissingJsonRpcField)?
            .as_str()
            .ok_or(MessageReaderError::JsonTypeMismatch)?;

        if rpc_version != "2.0" {
            return Err(MessageReaderError::WrongJsonRpcVersion);
        }

        let id = json
            .get("id")
            .map(|v| v.as_i64().expect("id is not a number!"));

        let error = json.get("error");
        let result = json.get("error");
        let params = json.get("params");
        let method = json.get("method");

        match (id, method, result, error, params) {
            // Request message
            (Some(id), Some(method), None, None, params) => {
                let method = method.as_str().unwrap();
                let params = params.cloned().ok_or(MessageReaderError::MissingParams);
                let method = match method {
                    Shutdown::METHOD => RequestMethod::Shutdown,
                    Initialize::METHOD => {
                        RequestMethod::Initialize(serde_json::from_value(params?)?)
                    }
                    _ => RequestMethod::Unknown(method.to_string()),
                };

                Ok(Message::Request(Request { id, method }))
            }
            // Success response message
            (Some(id), None, Some(result), None, None) => Ok(Message::Response(Response {
                id,
                result: ResponseKind::Success(result.clone()),
            })),
            // Error response message
            (Some(id), None, None, Some(error), None) => Ok(Message::Response(Response {
                id,
                result: ResponseKind::Error(error.clone()),
            })),
            // Notification
            (None, Some(method), None, None, params) => {
                Ok(Message::Notification(Notification::Unknown {
                    method: method.clone(),
                    params: params.cloned(),
                }))
            }
            _ => return Err(MessageReaderError::UnexpectedJsonRpcMessage),
        }
    }
}

impl<R: BufRead> Iterator for MessageReader<R> {
    type Item = Result<Message, MessageReaderError>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.next_impl();
        if matches!(result, Err(MessageReaderError::EOF)) {
            return None;
        }
        return Some(result);
    }
}

#[derive(Debug, Error)]
pub enum MessageReaderError {
    #[error("IO error: {0}")]
    IOError(#[from] io::Error),

    #[error("ParseIntError: {0}")]
    ParseError(#[from] ParseIntError),

    #[error("JSON parsing error: {0}")]
    SerdeJSON(#[from] serde_json::Error),

    #[error("UTF-8 error: {0}")]
    Utf8(#[from] Utf8Error),

    #[error("Missing Content-Length header")]
    MissingContentLenHeader,

    #[error("Missing jsonrpc field")]
    MissingJsonRpcField,

    #[error("Missing id field")]
    JsonTypeMismatch,

    #[error("Unexpected JSON-RPC version")]
    WrongJsonRpcVersion,

    #[error("Unexpected JSON-RPC message")]
    UnexpectedJsonRpcMessage,

    #[error("Missing expected params field")]
    MissingParams,

    #[error("EOF")]
    EOF,
}

use std::{
    io::{self, BufRead},
    num::ParseIntError,
};

use crate::messages::Message;

pub struct MessageReader<R> {
    input: R,
    buffer: String,
}

impl<R: BufRead> MessageReader<R> {
    pub fn new(input: R) -> Self {
        let buffer = String::with_capacity(512);
        Self { input, buffer }
    }

    fn next_impl(&mut self) -> Result<Message, MessageReaderError> {
        // Read headers
        let mut content_length = None;
        loop {
            let n = self.input.read_line(&mut self.buffer)?;

            if n == 0 {
                return Err(MessageReaderError::EOF);
            }

            if self.buffer == "\r" {
                break;
            }

            let mut header = self.buffer.split(": ");
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

        let mut buf = Vec::new();
        buf.resize(content_length, 0);

        self.input.read_exact(&mut buf)?;
        todo!()
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

#[derive(Debug)]
pub enum MessageReaderError {
    IOError(io::Error),
    ParseError(ParseIntError),
    MissingContentLenHeader,
    EOF,
}

impl From<io::Error> for MessageReaderError {
    fn from(value: io::Error) -> Self {
        MessageReaderError::IOError(value)
    }
}

impl From<ParseIntError> for MessageReaderError {
    fn from(value: ParseIntError) -> Self {
        MessageReaderError::ParseError(value)
    }
}

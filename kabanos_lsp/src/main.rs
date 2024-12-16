mod messages;
mod reader;

use std::io::{stdin, Read, Seek};

use std::io::{BufReader, Cursor};

use crate::{
    messages::request::{Request, RequestMethod},
    messages::Message,
    reader::MessageReader,
};

fn main() {
    let stdin = stdin().lock();
    let reader = MessageReader::new(stdin);
    for message in reader {
        dbg!(message);
    }

    let input_str =
        "Content-Length: 49\r\n\r\n{\"jsonrpc\": \"2.0\", \"id\": 1, \"method\": \"shutdown\"}";
    let cursor = Cursor::new(input_str);
    let buf_reader = BufReader::new(cursor);

    let mut message_reader = MessageReader::new(buf_reader);
    let message = message_reader.next().unwrap().unwrap();

    let mut remains = Vec::new();
    let n = message_reader.input.read_to_end(&mut remains).unwrap();
    assert_eq!(n, 0);

    let expected = Message::Request(Request {
        id: 1,
        method: RequestMethod::Shutdown,
    });

    assert_eq!(message, expected);
}

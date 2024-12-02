mod messages;
mod reader;

use std::io::stdin;

use reader::MessageReader;

fn main() {
    let stdin = stdin().lock();
    let reader = MessageReader::new(stdin);
    for message in reader {
        dbg!(message);
    }
}

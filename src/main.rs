pub mod lexer;
pub mod operator;
pub mod token;

use std::io::{self, BufRead};

use lexer::Lexer;

// fn reader_chars<R: BufRead>(reader: R) -> impl Iterator<Item = char> {
//     reader.lines().flat_map(|l| l).flat_map(|s| s.chars())
// }

fn main() {
    let stdin = io::stdin();
    let mut lock = stdin.lock();
    let mut buf = String::new();

    lock.read_line(&mut buf).unwrap();

    let lexer = Lexer::new(buf.chars());

    for token in lexer {
        println!("{:?}", token);
    }
}

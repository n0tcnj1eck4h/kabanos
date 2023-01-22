pub mod ast;
pub mod environment;
pub mod lexer;
pub mod operator;
pub mod parser;
pub mod token;
pub mod value;

use std::io::{self, BufRead};

use lexer::Lexer;
use parser::Parser;

// fn reader_chars<R: BufRead>(reader: R) -> impl Iterator<Item = char> {
//     reader.lines().flat_map(|l| l).flat_map(|s| s.chars())
// }

fn main() {
    let stdin = io::stdin();
    let mut lock = stdin.lock();
    let mut buf = String::new();

    while let Ok(n) = lock.read_line(&mut buf) {
        if n == 0 {
            break;
        }

        let lexer = Lexer::new(buf.chars());
        // for token in lexer {
        //     println!("{:?}", token);
        // }
        //
        let mut parser = Parser::new(lexer);
        let expr = parser.expression();
        println!("{:?}", expr);

        // if let Ok(expr) = expr {
        //     let env = Environment{};
        //     let val = expr.evaluate(&env).unwrap();
        //     println!("{:?}", val);
        // }

        buf.clear();
    }
}

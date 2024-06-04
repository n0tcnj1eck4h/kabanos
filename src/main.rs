pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod types;

use lexer::Lexer;
use parser::Parser;

use std::{
    env::args_os,
    fs::File,
    io::{self, BufRead, Read},
};

fn main() {
    if let Some(filename) = args_os().nth(1) {
        // reading from a file
        let mut file = File::open(filename).expect("Failed to open file");
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let lexer = Lexer::new(contents.chars());
        let mut parser = Parser::new(lexer);

        match parser.program() {
            Ok(statements) => {
                println!("program");
                for p in statements.function_definitions {
                    // p.pretty_print(1);
                }
            }
            Err(err) => println!("{:?}", err),
        }
    } else {
        // enter REPL
        let stdin = io::stdin();
        let mut lock = stdin.lock();
        let mut buf = String::new();

        while let Ok(n) = lock.read_line(&mut buf) {
            if n == 0 {
                break;
            }

            let lexer = Lexer::new(buf.chars());
            let mut parser = Parser::new(lexer);
            let stmt = parser.statement();
            println!("{:?}", stmt);

            buf.clear();
        }
    }
}

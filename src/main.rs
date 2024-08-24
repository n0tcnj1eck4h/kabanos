mod ast;
mod lexer;
mod parser;
mod semantic;
mod token;

use lexer::Lexer;
use parser::Parser;
use token::Token;

use std::{
    env::args_os,
    io::{self, BufRead, Write},
};

fn main() {
    if let Some(filename) = args_os().nth(1) {
        let contents = std::fs::read_to_string(filename).expect("Failed to read file");

        let lexer = Lexer::new(contents.chars());
        let tokens: Vec<Token> = lexer.into_iter().collect();
        for token in &tokens {
            println!("{:?}", token);
        }

        let mut parser = Parser::new(tokens.into_iter()).unwrap();

        match parser.module() {
            Ok(statements) => {
                println!("{:#?}", statements);
            }
            Err(err) => println!("{:?}", err),
        }
    } else {
        let stdin = io::stdin();
        let mut buf = String::new();

        loop {
            print!(">>> ");
            io::stdout().flush().expect("Failed to flush stdout");
            let mut lock = stdin.lock();
            if let Ok(n) = lock.read_line(&mut buf) {
                if n == 0 {
                    break;
                }
                let lexer = Lexer::new(buf.chars());
                // let mut parser = Parser::new(lexer);
                // let stmt = parser.statement();
                for token in lexer {
                    println!("{:?}", token);
                }

                buf.clear();
            }
        }
    }
}

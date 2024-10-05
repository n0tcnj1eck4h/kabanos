mod ast;
mod codegen;
mod lexer;
mod parser;
mod semantic;
// mod symbol;
mod token;

// use codegen::ModuleProvider;
use inkwell::context::Context;
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

        let mut parser = Parser::new(tokens.into_iter()).unwrap();

        match parser.module() {
            Ok(module) => {
                let module: semantic::Module = module.try_into().unwrap();
                let context = Context::create();
                module
                    .build_module(&context, "tmp")
                    .unwrap()
                    .print_to_file("out.ll")
                    .unwrap();
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

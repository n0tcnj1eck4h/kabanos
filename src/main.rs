// mod codegen;
mod ast;
mod lexer;
mod semantic;
mod span;
mod token;

// use codegen::ModuleProvider;
use ast::parser::Parser;
use lexer::Lexer;
use semantic::from_ast::Analyzer;
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

        let mut parser = Parser::new(tokens.into_iter()).expect("Stream is empty");

        let ast = match parser.module() {
            Ok(module) => module,
            Err(err) => return println!("Syntax error: {}", err),
        };

        let mut analyzer = Analyzer::default();
        let module = match analyzer.build_module(ast) {
            Ok(module) => module,
            Err(err) => return println!("Semantic error: {}", err),
        };

        dbg!(module);
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

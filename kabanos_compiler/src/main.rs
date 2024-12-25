// mod codegen;

// use codegen::ModuleProvider;
use kabanos_common::codegen::module::ModuleCodegen;
use kabanos_common::lexer::Lexer;
use kabanos_common::semantic::Module;
use kabanos_common::{ast::parser::Parser, semantic::module_builder};

use std::{
    env::args_os,
    io::{self, BufRead, Write},
};

fn main() {
    if let Some(filename) = args_os().nth(1) {
        let contents = std::fs::read_to_string(filename).expect("Failed to read file");

        let lexer = Lexer::new(contents.chars());
        let tokens: Vec<_> = lexer.into_iter().collect();

        let mut parser = Parser::new(tokens.into_iter()).expect("Stream is empty");

        let ast = match parser.module() {
            Ok(module) => module,
            Err(err) => return println!("Syntax error: {}", err),
        };

        let module = match Module::build_module(ast) {
            Ok(module) => module,
            Err(err) => return println!("Semantic error: {:?}", err),
        };

        let codegen = ModuleCodegen::new();
        let module = match codegen.build_module(module, "main") {
            Ok(module) => module,
            Err(err) => return println!("Codegen error: {:?}", err),
        };

        module
            .print_to_file("out.ll")
            .expect("Failed to write module to file");
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

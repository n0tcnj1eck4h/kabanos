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

use crate::{
    ast::{expression::Evaluate, statement::Execute},
    environment::Environment,
};

// fn reader_chars<R: BufRead>(reader: R) -> impl Iterator<Item = char> {
//     reader.lines().flat_map(|l| l).flat_map(|s| s.chars())
// }

fn main() {
    let stdin = io::stdin();
    let mut lock = stdin.lock();
    let mut buf = String::new();

    let env = Environment {};
    while let Ok(n) = lock.read_line(&mut buf) {
        if n == 0 {
            break;
        }

        let lexer = Lexer::new(buf.chars());
        let mut parser = Parser::new(lexer);
        let stmt = parser.statement();
        println!("{:?}", stmt);

        match stmt {
            Ok(mut stmt) => stmt.execute(&env).unwrap(),
            Err(err) => println!("{:?}", err),
        }

        // let expr = parser.expression();
        // println!("{:?}", expr);
        //
        // match expr {
        //     Ok(expr) => {
        //         let val = expr.evaluate(&env);
        //         match val {
        //             Ok(val) => println!("{:?}", val),
        //             Err(err)=> println!("Evaluation error: {:?}", err),
        //         }
        //     }
        //     Err(err) => {
        //         println!("Parsing error: {:?}", err);
        //     }
        // }

        buf.clear();
    }
}

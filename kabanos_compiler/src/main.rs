use std::fmt::Display;

use kabanos_common::ast::parser::Parser;
use kabanos_common::codegen::module::ModuleCodegen;
use kabanos_common::lexer::Lexer;
use kabanos_common::semantic::Module;
use kabanos_common::span::HasSpan;

fn main() {
    let filename = std::env::args_os().nth(1).expect("Bad usage");
    let contents = std::fs::read_to_string(filename).expect("Failed to read file");

    let lexer = Lexer::new(contents.chars());
    let tokens: Result<Vec<_>, _> = lexer.collect();
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(err) => return pretty_print_err(err, &contents),
    };

    let mut parser = Parser::new(tokens.into_iter()).expect("Stream is empty");
    let ast = match parser.module() {
        Ok(ast) => ast,
        Err(err) => return pretty_print_err(err, &contents),
    };

    std::fs::write("ast.txt", format!("{:#?}", ast)).expect("Failed to write ast.txt");

    let semantic_module = match Module::build_module(ast) {
        Ok(semantic_module) => semantic_module,
        Err(err) => {
            return err
                .into_iter()
                .for_each(|err| pretty_print_err(err, &contents))
        }
    };

    std::fs::write("semantic.txt", format!("{:#?}", semantic_module))
        .expect("Failed to write semantic.txt");

    let codegen = ModuleCodegen::default();
    let llvm_module = match codegen.build_module(semantic_module, "main") {
        Ok(llvm_module) => llvm_module,
        Err(err) => return println!("Codegen error: {}", err),
    };

    llvm_module
        .print_to_file("out.ll")
        .expect("Failed to write module to file");
}

fn pretty_print_err(err: impl Display + HasSpan, code: &str) {
    let span = err.get_span();
    let start_line = span.start.row as isize;
    let end_line = span.end.row as isize;
    let msg = err.to_string();
    println!(
        "\\begin{{lstlisting}}[caption={{{}}}, label={{lst:{}}}]",
        msg, "err000"
    );
    for (i, line) in code.lines().enumerate() {
        let i = i as isize;
        if start_line - i < 2 {
            println!("{}", line);
        } else if i - end_line < 2 {
            println!("{}", line);
        }
        if i == end_line {
            let start_col = span.start.col as isize;
            let end_col = span.end.col as isize;
            for _ in 0..start_col {
                print!(" ");
            }
            for _ in start_col..end_col {
                print!("^");
            }
            println!();
            for _ in 0..start_col {
                print!(" ");
            }
            println!("{}", err);
        }
    }
    println!("\\end{{lstlisting}}");
}

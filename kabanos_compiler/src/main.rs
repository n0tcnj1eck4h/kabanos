use std::fmt::Display;
use std::path::Path;

use colored::Colorize;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;
use kabanos_codegen::module::CodegenContext;
use kabanos_common::ast::parser::Parser;
use kabanos_common::lexer::Lexer;
use kabanos_common::semantic::Module;
use kabanos_common::span::HasSpan;

fn main() {
    let filename = std::env::args_os().nth(1).expect("Bad usage");
    let contents = std::fs::read_to_string(filename).expect("Failed to read file");

    println!("{}", "Lexing...".green());
    let lexer = Lexer::new(contents.chars());
    let tokens: Result<Vec<_>, _> = lexer.collect();
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(err) => return pretty_print_err(err, &contents),
    };

    println!("{}", "Parsing...".green());
    let mut parser = Parser::new(tokens.into_iter()).expect("Stream is empty");
    let ast = match parser.module() {
        Ok(ast) => ast,
        Err(err) => return pretty_print_err(err, &contents),
    };

    std::fs::write("ast.txt", format!("{:#?}", ast)).expect("Failed to write ast.txt");

    println!("{}", "Analyzing...".green());
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

    println!("{}", "Emitting LLVM...".green());
    let codegen = CodegenContext::default();
    let llvm_module = match codegen.build_module(semantic_module, "main") {
        Ok(llvm_module) => llvm_module,
        Err(err) => return println!("Codegen error: {}", err),
    };

    llvm_module
        .print_to_file("out.ll")
        .expect("Failed to write module to file");

    Target::initialize_x86(&InitializationConfig::default());
    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-linux-gnu"),
            "generic",
            "",
            opt,
            reloc,
            model,
        )
        .unwrap();

    println!("{}", "Compiling...".green());
    target_machine
        .write_to_file(&llvm_module, FileType::Object, Path::new("out.o"))
        .expect("Failed to write object file");

    println!("{}", "Done!".bold().green());
}

fn pretty_print_err(err: impl Display + HasSpan, code: &str) {
    let span = err.get_span();
    let start_line = span.start.row as isize;
    let end_line = span.end.row as isize;
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
}

use kabanos_common::ast::parser::Parser;
use kabanos_common::codegen::module::ModuleCodegen;
use kabanos_common::lexer::Lexer;
use kabanos_common::semantic::Module;

fn main() {
    let filename = std::env::args_os().nth(1).expect("Bad usage");
    let contents = std::fs::read_to_string(filename).expect("Failed to read file");

    let lexer = Lexer::new(contents.chars());
    let mut parser = Parser::new(lexer).expect("Stream is empty");

    let ast = match parser.module() {
        Ok(ast) => ast,
        Err(err) => return println!("Syntax error: {}", err),
    };

    let semantic_module = match Module::build_module(ast) {
        Ok(semantic_module) => semantic_module,
        Err(err) => return println!("Semantic error: {:?}", err),
    };

    let codegen = ModuleCodegen::new();
    let llvm_module = match codegen.build_module(semantic_module, "main") {
        Ok(llvm_module) => llvm_module,
        Err(err) => return println!("Codegen error: {}", err),
    };

    llvm_module
        .print_to_file("out.ll")
        .expect("Failed to write module to file");
}

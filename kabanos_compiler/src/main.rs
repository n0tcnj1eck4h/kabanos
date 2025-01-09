use kabanos_common::ast::parser::Parser;
use kabanos_common::codegen::module::ModuleCodegen;
use kabanos_common::lexer::Lexer;
use kabanos_common::semantic::Module;

fn main() {
    let filename = std::env::args_os().nth(1).expect("Bad usage");
    let contents = std::fs::read_to_string(filename).expect("Failed to read file");

    let lexer = Lexer::new(contents.chars());
    let tokens: Result<Vec<_>, _> = lexer.collect();
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(err) => return println!("Lexing error: {:?}", err),
    };

    let mut parser = Parser::new(tokens.into_iter()).expect("Stream is empty");
    let ast = match parser.module() {
        Ok(ast) => ast,
        Err(err) => return println!("Syntax error: {}", err),
    };

    std::fs::write("ast.txt", format!("{:#?}", ast)).expect("Failed to write ast.txt");

    let semantic_module = match Module::build_module(ast) {
        Ok(semantic_module) => semantic_module,
        Err(err) => return println!("Semantic error: {:?}", err),
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

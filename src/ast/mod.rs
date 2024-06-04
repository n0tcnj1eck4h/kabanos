pub mod expression;
pub mod statement;

use statement::Statement;

pub struct Program {
    pub imports: Vec<String>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub globals: Vec<GlobalVariableDefintion>,
}

pub struct FunctionDefinition {
    name: String,
    body: Vec<Statement>,
}

pub struct GlobalVariableDefintion {
    name: String,
}

pub struct Import {
    pub path: String,
}

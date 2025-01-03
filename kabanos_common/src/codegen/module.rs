use std::collections::HashMap;

use inkwell::{
    context::{self, Context},
    module::Module,
    types::BasicType,
    values::{FunctionValue, PointerValue},
};

use crate::semantic::{self, symbol::VariableID};

use super::{error::CodegenResult, statement::Codegen};

pub struct ModuleCodegen {
    pub context: Context,
}

impl ModuleCodegen {
    pub fn new() -> Self {
        Self {
            context: Context::create(),
        }
    }

    pub fn build_module<'ctx>(
        &'ctx self,
        semantic_module: semantic::Module,
        name: &str,
    ) -> CodegenResult<Module<'ctx>> {
        let module = self.context.create_module(name);
        let mut symbol_table = semantic_module.symbol_table;
        let mut functions = HashMap::new();

        // Declare functions
        for fn_id in symbol_table.iterate_functions() {
            let fn_dec = symbol_table.get_function(fn_id);
            let function = self.build_function_prototype(&module, fn_dec);
            functions.insert(fn_id, function);
            println!("declaring {}", fn_dec.name);
        }

        // Define functions
        for fn_id in symbol_table.iterate_functions() {
            let builder = self.context.create_builder();
            let function = *functions.get(&fn_id).expect("oops4");
            if let Some(fn_def) = symbol_table.pop_function_body(fn_id) {
                let block = self.context.append_basic_block(function, "entry");
                builder.position_at_end(block);

                let mut variables = HashMap::<VariableID, PointerValue<'ctx>>::new();
                for (param, id) in function.get_params().into_iter().zip(&fn_def.params) {
                    let p = symbol_table.get_variable(*id);
                    param.set_name(&p.identifier);

                    let ptr = builder.build_alloca(param.get_type(), "param_ptr")?;
                    builder.build_store(ptr, param)?;

                    variables.insert(*id, ptr);
                }

                let mut codegen = Codegen {
                    context: &self.context,
                    builder,
                    function,
                    variables,
                    symbol_table: &symbol_table,
                    functions: &functions,
                };

                for statement in fn_def.body {
                    codegen.build_statement(statement)?;
                }
            }
        }
        Ok(module)
    }

    fn build_function_prototype<'ctx>(
        &'ctx self,
        module: &Module<'ctx>,
        fn_decl: &semantic::FunctionDeclaration,
    ) -> FunctionValue<'ctx> {
        let mut params = Vec::new();
        for param in &fn_decl.params {
            params.push(param.ty.to_llvm_type(&self.context).into());
        }

        let fn_type = match fn_decl.ty {
            Some(t) => {
                let return_type = t.to_llvm_type(&self.context);
                return_type.fn_type(&params, false)
            }
            None => self.context.void_type().fn_type(&params, false),
        };

        module.add_function(&fn_decl.name, fn_type, None)
    }
}

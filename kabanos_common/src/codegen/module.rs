use std::collections::HashMap;

use inkwell::{
    context::Context,
    module::Module,
    types::BasicType,
    values::{FunctionValue, PointerValue},
};

use crate::semantic::{
    self,
    symbol::{SymbolTable, VariableID},
};

use super::error::CodegenResult;

pub struct ModuleCoden<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    variables: HashMap<VariableID, PointerValue<'ctx>>,
}

impl ModuleCoden<'_> {
    pub fn build_module<'ctx>(
        &self,
        module: semantic::Module,
        name: &str,
    ) -> CodegenResult<Module<'ctx>> {
        let builder = self.context.create_builder();

        for fn_id in module.symbol_table.iterate_functions() {
            let fn_dec = module.symbol_table.get_function(fn_id);
            let function = self.build_function_prototype(fn_dec, &module.symbol_table);

            if let Some(fn_body) = module.symbol_table.get_function_body(fn_id) {
                let block = self.context.append_basic_block(function, "entry");
                builder.position_at_end(block);

                for (param, id) in function.get_params().into_iter().zip(&fn_dec.params) {
                    let p = module.symbol_table.get_variable(*id);
                    param.set_name(&p.identifier);

                    let ptr = builder.build_alloca(param.get_type(), "param_ptr")?;
                    builder.build_store(ptr, param)?;

                    self.variables.insert(*id, ptr);
                }

                for statement in fn_body {
                    statement.build_statement(
                        self.context,
                        &builder,
                        function,
                        &mut self.module,
                    )?;
                }
            }
        }

        Ok(self.module)
    }

    fn build_function_prototype<'ctx>(
        &'ctx self,
        fn_decl: &semantic::FunctionDeclaration,
        symbol_table: &SymbolTable,
    ) -> FunctionValue<'ctx> {
        let mut params = Vec::new();
        for param_id in &fn_decl.params {
            let param = symbol_table.get_variable(*param_id);
            params.push(param.ty.to_llvm_type(self.context).into());
        }

        let fn_type = match fn_decl.ty {
            Some(t) => {
                let return_type = t.to_llvm_type(self.context);
                return_type.fn_type(&params, false)
            }
            None => self.context.void_type().fn_type(&params, false),
        };

        self.module.add_function(&fn_decl.name, fn_type, None)
    }
}

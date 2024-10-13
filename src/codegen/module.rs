use inkwell::{context::Context, module::Module, types::BasicType, values::FunctionValue};

use crate::semantic;

use super::{
    error::CodegenResult,
    symbol_table::{Symbol, SymbolTable},
};

impl semantic::Module {
    pub fn build_module<'ctx>(
        &self,
        context: &'ctx Context,
        name: &str,
    ) -> CodegenResult<Module<'ctx>> {
        let builder = context.create_builder();
        let module = context.create_module(name);
        let mut symbol_table = SymbolTable::default();

        for fn_dec in &self.declarations {
            let function = fn_dec.build_function_prototype(context, &module);
            symbol_table.add_function(fn_dec.name.clone(), function);
        }

        for fn_def in &self.functions {
            let function = fn_def
                .declaration
                .build_function_prototype(context, &module);
            symbol_table.add_function(fn_def.declaration.name.clone(), function);
        }

        for fn_def in &self.functions {
            symbol_table.push_scope();

            let function = symbol_table.get_function(&fn_def.declaration.name).unwrap();
            let block = context.append_basic_block(function, "entry");
            builder.position_at_end(block);

            for (i, p) in fn_def.declaration.params.iter().enumerate() {
                let param = function.get_nth_param(i as u32).unwrap();
                param.set_name(&p.name);

                let ptr = builder.build_alloca(param.get_type(), "param_ptr")?;
                builder.build_store(ptr, param)?;

                let ty = param.get_type();

                let symbol = Symbol { ptr, ty };

                symbol_table.push_value(&p.name, symbol);
            }

            for statement in &fn_def.body {
                statement.build_statement(context, &builder, function, &mut symbol_table)?;
            }

            symbol_table.pop_scope();
        }

        Ok(module)
    }
}

impl semantic::FunctionDeclaration {
    fn build_function_prototype<'ctx>(
        &self,
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) -> FunctionValue<'ctx> {
        let mut params = Vec::new();
        for param in self.params.iter() {
            params.push(param.ty.to_llvm_type(context).into());
        }

        let fn_type = match self.ty {
            Some(t) => {
                let return_type = t.to_llvm_type(context);
                return_type.fn_type(&params, false)
            }
            None => context.void_type().fn_type(&params, false),
        };

        module.add_function(&self.name, fn_type, None)
    }
}

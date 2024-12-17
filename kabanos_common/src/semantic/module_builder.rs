use std::str::FromStr;

use super::{
    error::SemanticError, primitive::Primitive, statement_builder::StatementBuilder,
    symbol::Variable, FunctionDeclaration, Module, Parameter,
};
use crate::ast;

impl Module {
    pub fn build_module(module: ast::Module) -> Result<Module, SemanticError> {
        let mut context = Module::default();

        for s in module.fn_declarations {
            let span = s.span;
            let fn_decl = Self::build_declaration(s)?;
            context
                .symbol_table
                .declare_function(fn_decl)
                .map_err(|e| e.with_span(span))?;
        }

        for s in module.fn_definitions.iter() {
            let fn_decl = Self::build_declaration(s.prototype.clone())?;
            context
                .symbol_table
                .declare_function(fn_decl)
                .map_err(|e| e.with_span(s.prototype.span))?;
        }

        for s in module.fn_definitions {
            let span = s.prototype.span;
            let declaration = Self::build_declaration(s.prototype)?;

            let mut stack = Vec::new();
            for param in &declaration.params {
                let id = context.symbol_table.push_local_var(Variable {
                    identifier: param.name.clone(),
                    ty: param.ty,
                });
                stack.push(id);
            }

            let mut statement_builder = StatementBuilder {
                symbol_table: &mut context.symbol_table,
                expected_return_ty: declaration.ty,
                stack: &mut stack,
            };

            let body = statement_builder.build_statements(s.body)?;
            let decl_id = context
                .symbol_table
                .get_function_id_by_decl(&declaration)
                .map_err(|e| e.with_span(span))?
                .expect("Function not forward declared");
            context
                .symbol_table
                .define_function(decl_id, body)
                .map_err(|e| e.with_span(span))?;
        }

        Ok(context)
    }

    fn build_declaration(
        prototype: ast::FunctionPrototype,
    ) -> Result<FunctionDeclaration, SemanticError> {
        let name = prototype.name;
        let ty = prototype
            .return_type
            .map(|ty| Primitive::from_str(&ty))
            .transpose()
            .map_err(|e| e.with_span(prototype.span))?
            .map(Into::into);

        let mut params = Vec::new();
        for p in prototype.parameters {
            let name = p.name;
            let ty = Primitive::from_str(&p.ty)
                .map_err(|e| e.with_span(p.span))?
                .into();
            let param = Parameter { name, ty };
            params.push(param);
        }

        Ok(FunctionDeclaration { name, ty, params })
    }
}

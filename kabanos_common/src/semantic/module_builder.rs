use std::str::FromStr;

use super::{
    error::SemanticError, primitive::Primitive, statement_builder::StatementBuilder,
    symbol::Variable, FunctionDeclaration, Module, Parameter,
};
use crate::{
    ast::{self, FunctionDefinition, FunctionPrototype},
    span::{Spanned, WithSpan as _},
};

impl Module {
    pub fn build_module(ast_module: ast::Module) -> Result<Module, Vec<Spanned<SemanticError>>> {
        let mut module = Module::default();
        let mut errors = Vec::new();

        for s in ast_module.fn_declarations {
            if let Err(err) = module.build_delaration(s) {
                errors.push(err);
            }
        }

        for s in ast_module.fn_definitions.iter() {
            if let Err(err) = module.build_delaration(s.prototype.clone()) {
                errors.push(err);
            }
        }

        for s in ast_module.fn_definitions {
            if let Err(err) = module.build_definition(s) {
                errors.push(err);
            }
        }

        if errors.is_empty() {
            Ok(module)
        } else {
            Err(errors)
        }
    }

    fn build_delaration(&mut self, s: FunctionPrototype) -> Result<(), Spanned<SemanticError>> {
        let span = s.span;
        let fn_decl = Self::build_declaration(s)?;
        self.symbol_table
            .declare_function(fn_decl)
            .map_err(|e| e.with_span(span))?;
        Ok(())
    }

    fn build_definition(&mut self, s: FunctionDefinition) -> Result<(), Spanned<SemanticError>> {
        let span = s.prototype.span;
        let declaration = Self::build_declaration(s.prototype)?;

        let mut stack = Vec::new();
        for param in &declaration.params {
            let id = self.symbol_table.push_local_var(Variable {
                identifier: param.name.clone(),
                ty: param.ty,
            });
            stack.push(id);
        }

        let mut statement_builder = StatementBuilder {
            symbol_table: &mut self.symbol_table,
            expected_return_ty: declaration.ty,
            stack: &mut stack,
        };

        let body = statement_builder.build_statements(s.body)?;
        let decl_id = self
            .symbol_table
            .get_function_id_by_decl(&declaration)
            .map_err(|e| e.with_span(span))?
            .expect("Function not forward declared");
        self.symbol_table
            .define_function(decl_id, body)
            .map_err(|e| e.with_span(span))?;
        Ok(())
    }

    fn build_declaration(
        prototype: ast::FunctionPrototype,
    ) -> Result<FunctionDeclaration, Spanned<SemanticError>> {
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

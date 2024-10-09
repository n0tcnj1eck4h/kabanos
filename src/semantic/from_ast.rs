use crate::ast;

use super::*;

impl TryFrom<ast::Module> for Module {
    type Error = SemanticError;

    fn try_from(value: ast::Module) -> Result<Self, Self::Error> {
        let mut functions = Vec::new();
        for s in value.function_definitions {
            let s = s.try_into()?;
            functions.push(s);
        }

        let mut declarations = Vec::new();
        for s in value.function_declarations {
            let s = s.try_into()?;
            declarations.push(s);
        }

        Ok(Self {
            functions,
            declarations,
        })
    }
}

impl TryFrom<ast::FunctionDeclaration> for FunctionDeclaration {
    type Error = SemanticError;

    fn try_from(declaration: ast::FunctionDeclaration) -> Result<Self, Self::Error> {
        let name = declaration.name;

        let mut params = Vec::new();
        for param in declaration.parameters {
            let param = param.try_into()?;
            params.push(param);
        }

        let ty = declaration
            .return_type
            .map(|ty| FromStr::from_str(&ty))
            .transpose()?;

        Ok(Self { name, params, ty })
    }
}

impl TryFrom<ast::FunctionDefinition> for FunctionDefinition {
    type Error = SemanticError;
    fn try_from(function: ast::FunctionDefinition) -> Result<Self, Self::Error> {
        let mut body = Vec::new();
        for statement in function.body {
            let statement = statement.try_into()?;
            body.push(statement);
        }

        let declaration = function.declaration.try_into()?;

        Ok(Self { declaration, body })
    }
}

impl TryFrom<ast::Statement> for Statement {
    type Error = SemanticError;
    fn try_from(value: ast::Statement) -> Result<Self, Self::Error> {
        match value {
            ast::Statement::Conditional(expr, true_block, else_block) => {
                Ok(Statement::Conditional(
                    expr.try_into()?,
                    Box::new((*true_block).try_into()?),
                    else_block
                        .map(|eb| (*eb).try_into())
                        .transpose()?
                        .map(Box::new),
                ))
            }
            ast::Statement::Expression(expr) => Ok(Statement::Expression(expr.try_into()?)),
            ast::Statement::Loop(expr, statement) => Ok(Statement::Loop(
                expr.try_into()?,
                Box::new((*statement).try_into()?),
            )),
            ast::Statement::Block(statements) => {
                let mut r = Vec::new();
                for s in statements {
                    let s = s.try_into()?;
                    r.push(s);
                }
                Ok(Self::Block(r))
            }
            ast::Statement::Return(expr) => {
                Ok(Self::Return(expr.map(TryInto::try_into).transpose()?))
            }
            ast::Statement::LocalVar(identifier, ty, expr) => {
                if let Some(ty) = ty {
                    Ok(Self::LocalVar(
                        identifier,
                        ty.parse()?,
                        expr.map(TryInto::try_into).transpose()?,
                    ))
                } else {
                    Err(SemanticError::MissingExplicitType)
                }
            }
        }
    }
}

impl TryFrom<ast::Expression> for Expression {
    type Error = SemanticError;
    fn try_from(value: ast::Expression) -> Result<Self, Self::Error> {
        match value {
            ast::Expression::Identifier(ident) => Ok(Self::LValue(LValue::Identifier(ident))),
            ast::Expression::FunctionCall(name, arguments) => {
                let mut v = Vec::new();
                for a in arguments {
                    v.push(a.try_into()?);
                }
                Ok(Self::FunctionCall(name, v))
            }
            ast::Expression::StringLiteral(_) => unimplemented!(),
            ast::Expression::IntegerLiteral(int) => Ok(Self::IntegerLiteral(int)),
            ast::Expression::BooleanLiteral(bool) => Ok(Self::BooleanLiteral(bool)),
            ast::Expression::FloatingPointLiteral(float) => Ok(Self::FloatLiteral(float)),
            ast::Expression::UnaryOperation(op, expr) => {
                let op = op.try_into()?;
                let expr = Box::new((*expr).try_into()?);
                Ok(Self::UnaryOperation(op, expr))
            }
            ast::Expression::BinaryOperation(l, op, r) => {
                if op == Operator::Assign {
                    let l = (*l).try_into()?;
                    let r = Box::new((*r).try_into()?);
                    Ok(Self::Assignment(l, r))
                } else {
                    let op = op.try_into()?;
                    let l = Box::new((*l).try_into()?);
                    let r = Box::new((*r).try_into()?);
                    Ok(Self::BinaryOperation(l, op, r))
                }
            }
        }
    }
}

impl TryFrom<ast::Expression> for LValue {
    type Error = SemanticError;
    fn try_from(value: ast::Expression) -> Result<Self, Self::Error> {
        match value {
            ast::Expression::Identifier(ident) => Ok(Self::Identifier(ident)),
            _ => Err(SemanticError::LValue(value)),
        }
    }
}

impl TryFrom<ast::Parameter> for Parameter {
    type Error = SemanticError;
    fn try_from(value: ast::Parameter) -> Result<Self, Self::Error> {
        let name = value.name;
        let ty = value.ty.parse()?;
        Ok(Self { ty, name })
    }
}

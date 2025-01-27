use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    values::{BasicValueEnum, CallSiteValue, FloatValue, FunctionValue, IntValue, PointerValue},
    FloatPredicate, IntPredicate,
};

use crate::semantic::{
    self,
    expression::{Expression, LValue},
    operator::{ArithmeticOp, BinaryOperator, BitwiseOp, ComparaisonOp, LogicOp},
    symbol::{FunctionID, SymbolTable, VariableID},
    types::{IntTy, Type},
    FunctionCall, Scope,
};

use super::error::CodegenResult;

pub struct Codegen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub function: FunctionValue<'ctx>,
    pub symbol_table: &'a SymbolTable,
    pub variables: HashMap<VariableID, PointerValue<'ctx>>,
    pub functions: &'a HashMap<FunctionID, FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx>
where
    'a: 'ctx,
{
    pub fn build_statement(&mut self, stmt: semantic::Statement) -> CodegenResult {
        use semantic::Statement;
        match stmt {
            Statement::Conditional(condition, body, body_else) => {
                self.build_conditional(condition, body, body_else)
            }
            Statement::Loop(condition, body) => self.build_loop(condition, body),
            Statement::Scope(Scope { variable_id, body }) => self.build_scope(variable_id, body),
            Statement::Return(expression) => self.build_return(expression),
            Statement::Expression(expression) => {
                self.build_expression(expression)?;
                Ok(())
            }
            Statement::VoidFunctionCall(call) => {
                self.build_function_call(call)?;
                Ok(())
            }
        }
    }

    fn build_return(
        &mut self,
        expression: Option<Expression>,
    ) -> Result<(), super::error::IRBuilerError> {
        if let Some(expression) = expression {
            let ret_value = self.build_expression(expression)?;
            self.builder.build_return(Some(&ret_value))?;
        } else {
            self.builder.build_return(None)?;
        }
        Ok(())
    }

    fn build_scope(
        &mut self,
        variable_id: VariableID,
        body: Vec<semantic::Statement>,
    ) -> Result<(), super::error::IRBuilerError> {
        let variable = self.symbol_table.get_variable(variable_id);
        let ty = variable.ty.to_llvm_type(self.context);
        let ptr = self.builder.build_alloca(ty, &variable.identifier)?;
        self.variables.insert(variable_id, ptr);
        for statement in body {
            self.build_statement(statement)?;
        }
        Ok(())
    }

    fn build_loop(
        &mut self,
        condition: Expression,
        body: Vec<semantic::Statement>,
    ) -> Result<(), super::error::IRBuilerError> {
        let loop_block = self.context.append_basic_block(self.function, "loop");
        let body_block = self.context.append_basic_block(self.function, "body");
        let continue_block = self.context.append_basic_block(self.function, "continue");
        self.builder.build_unconditional_branch(loop_block)?;
        self.builder.position_at_end(loop_block);
        let condition = self.build_expression(condition)?;
        self.builder.build_conditional_branch(
            condition.into_int_value(),
            body_block,
            continue_block,
        )?;
        self.builder.position_at_end(body_block);
        for statement in body {
            self.build_statement(statement)?;
        }
        self.builder.build_unconditional_branch(loop_block)?;
        self.builder.position_at_end(continue_block);
        Ok(())
    }

    fn build_conditional(
        &mut self,
        condition: Expression,
        body: Vec<semantic::Statement>,
        body_else: Option<Vec<semantic::Statement>>,
    ) -> Result<(), super::error::IRBuilerError> {
        let condition = self.build_expression(condition)?;
        let then_block = self.context.append_basic_block(self.function, "then");
        let else_block = self.context.append_basic_block(self.function, "else");
        let merge_block = self.context.append_basic_block(self.function, "merge");
        self.builder.build_conditional_branch(
            condition.into_int_value(),
            then_block,
            else_block,
        )?;
        self.builder.position_at_end(then_block);
        for statement in body {
            self.build_statement(statement)?;
        }
        self.builder.build_unconditional_branch(merge_block)?;
        self.builder.position_at_end(else_block);
        if let Some(body_else) = body_else {
            for statement in body_else {
                self.build_statement(statement)?;
            }
        }
        self.builder.build_unconditional_branch(merge_block)?;
        self.builder.position_at_end(merge_block);
        Ok(())
    }

    pub fn build_expression(&self, expr: Expression) -> CodegenResult<BasicValueEnum<'_>> {
        match expr {
            Expression::Assignment(LValue::LocalVar(variable_id), expr) => {
                self.build_assign_expr(variable_id, expr)
            }
            Expression::LValue(LValue::LocalVar(variable_id)) => {
                self.build_lvalue_expr(variable_id)
            }
            Expression::BooleanLiteral(b) => {
                Ok(self.context.bool_type().const_int(b as u64, false).into())
            }
            Expression::IntegerLiteral(int, ty) => Ok(Type::Int(ty)
                .to_llvm_type(self.context)
                .into_int_type()
                .const_int(int, false)
                .into()),
            Expression::FloatLiteral(f, ty) => Ok(Type::Float(ty)
                .to_llvm_type(self.context)
                .into_float_type()
                .const_float(f)
                .into()),
            Expression::BinaryOperation(lexpr, op, rexpr) => self.build_binop(lexpr, op, rexpr),
            Expression::UnaryOperation(_op, _expr) => {
                // TODO
                //Ok(expr.build_expression(context, builder, symbol_table)?)
                todo!()
            }
            Expression::FunctionCall(call) => {
                let call_site = self.build_function_call(call)?;
                Ok(call_site.try_as_basic_value().unwrap_left())
            }
            Expression::Cast(expr, to) => self.build_cast(expr, to),
        }
    }

    fn build_lvalue_expr(
        &self,
        variable_id: VariableID,
    ) -> Result<BasicValueEnum<'_>, super::error::IRBuilerError> {
        let variable = self.variables.get(&variable_id).expect("oops2");
        let symbol = self.symbol_table.get_variable(variable_id);
        let ty = symbol.ty.to_llvm_type(self.context);
        Ok(self.builder.build_load(ty, *variable, &symbol.identifier)?)
    }

    fn build_cast(
        &self,
        expr: Box<Expression>,
        to: Type,
    ) -> Result<BasicValueEnum<'_>, super::error::IRBuilerError> {
        let from = self.symbol_table.get_expression_type(&expr);
        let value = self.build_expression(*expr)?;
        let ty = to.to_llvm_type(self.context);
        if from == to {
            return Ok(value);
        }
        use Type::*;
        Ok(match (&from, to) {
            (Int(from), Int(to)) => {
                let value = value.into_int_value();
                let ty = ty.into_int_type();
                match (from.bits < to.bits, to.sign) {
                    (true, true) => self.builder.build_int_s_extend(value, ty, "sext")?,
                    (true, false) => self.builder.build_int_z_extend(value, ty, "zext")?,
                    (false, _) => self.builder.build_int_truncate(value, ty, "trunc")?,
                }
                .into()
            }
            (Int(from), Float(_)) => {
                let value = value.into_int_value();
                let ty = ty.into_float_type();
                if from.sign {
                    self.builder
                        .build_signed_int_to_float(value, ty, "sitofp")?
                } else {
                    self.builder
                        .build_unsigned_int_to_float(value, ty, "uitofp")?
                }
                .into()
            }
            (Int(_), Bool) => {
                let from = from.to_llvm_type(self.context).into_int_type();
                let value = value.into_int_value();
                let zero = from.const_int(0, false);
                self.builder
                    .build_int_compare(IntPredicate::NE, value, zero, "int_to_bool")?
                    .into()
            }
            (Float(_), Int(to)) => {
                let value = value.into_float_value();
                let ty = ty.into_int_type();
                if to.sign {
                    self.builder
                        .build_float_to_signed_int(value, ty, "fptosi")?
                } else {
                    self.builder
                        .build_float_to_unsigned_int(value, ty, "fptoui")?
                }
                .into()
            }
            (Float(from), Float(to)) => {
                let value = value.into_float_value();
                let ty = ty.into_float_type();
                if *from < to {
                    self.builder.build_float_ext(value, ty, "fext")?
                } else {
                    self.builder.build_float_trunc(value, ty, "ftruc")?
                }
                .into()
            }
            (Float(_), Bool) => {
                let from = from.to_llvm_type(self.context).into_float_type();
                let value = value.into_float_value();
                let zero = from.const_float(0.0);
                self.builder
                    .build_float_compare(FloatPredicate::UNE, value, zero, "float_to_bool")?
                    .into()
            }
            (Bool, Int(_)) => {
                let value = value.into_int_value();
                let ty = ty.into_int_type();
                self.builder.build_int_z_extend(value, ty, "zext")?.into()
            }
            (Bool, Float(_)) => {
                let value = value.into_int_value();
                let ty = ty.into_float_type();
                self.builder
                    .build_unsigned_int_to_float(value, ty, "uitofp")?
                    .into()
            }
            (Bool, Bool) => value,
            (Ptr(_), _) => todo!(),
            (_, Ptr(_)) => todo!(),
        })
    }

    fn build_binop(
        &self,
        lexpr: Box<Expression>,
        op: BinaryOperator,
        rexpr: Box<Expression>,
    ) -> Result<BasicValueEnum<'_>, super::error::IRBuilerError> {
        let ty = self.symbol_table.get_expression_type(&lexpr);
        let l = self.build_expression(*lexpr)?;
        let r = self.build_expression(*rexpr)?;
        Ok(match op {
            BinaryOperator::Logic(logic_op) => self
                .build_logic_binop(logic_op, l.into_int_value(), r.into_int_value())?
                .into(),
            BinaryOperator::Bitwise(bitwise_op) => self
                .build_bitwise_binop(bitwise_op, l.into_int_value(), r.into_int_value())?
                .into(),
            BinaryOperator::Arithmetic(arithmetic_op) => {
                let sign = matches!(ty, Type::Int(IntTy { sign: true, .. }));
                self.build_arithmetic_binop(arithmetic_op, l, r, sign)?
            }
            BinaryOperator::Comparaison(op) => match ty {
                Type::Int(ty) => {
                    self.build_int_cmp_binop(op, l.into_int_value(), r.into_int_value(), ty.sign)?
                }
                Type::Float(_) => {
                    self.build_float_cmp_binop(op, l.into_float_value(), r.into_float_value())?
                }
                Type::Bool => {
                    self.build_int_cmp_binop(op, l.into_int_value(), r.into_int_value(), false)?
                }
                Type::Ptr(_) => todo!(),
            },
        })
    }

    fn build_assign_expr(
        &self,
        variable_id: VariableID,
        expr: Box<Expression>,
    ) -> Result<BasicValueEnum<'_>, super::error::IRBuilerError> {
        let r = self.build_expression(*expr)?;
        let ptr = self.variables.get(&variable_id).expect("oops");
        let symbol = self.symbol_table.get_variable(variable_id);
        let ty = ptr.get_type();
        self.builder.build_store(*ptr, r)?;
        Ok(self.builder.build_load(ty, *ptr, &symbol.identifier)?)
    }

    pub fn build_function_call(&'a self, call: FunctionCall) -> CodegenResult<CallSiteValue<'ctx>> {
        let fn_decl = self.symbol_table.get_function(call.id);
        let fn_value = self.functions.get(&call.id).expect("oops3");
        let mut args = Vec::new();
        for a in call.args {
            let a = self.build_expression(a)?;
            args.push(a.into());
        }
        Ok(self.builder.build_call(*fn_value, &args, &fn_decl.name)?)
    }

    pub fn build_arithmetic_binop(
        &'a self,
        op: ArithmeticOp,
        l: BasicValueEnum<'ctx>,
        r: BasicValueEnum<'ctx>,
        sign: bool,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        use ArithmeticOp::*;
        let builder = &self.builder;
        match (l, r) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(match op {
                Add => builder.build_int_add(l, r, "add")?.into(),
                Subtract => builder.build_int_sub(l, r, "sub")?.into(),
                Multiply => builder.build_int_mul(l, r, "mul")?.into(),
                Divide if sign => builder.build_int_signed_div(l, r, "div")?.into(),
                Modulo if sign => builder.build_int_signed_rem(l, r, "srem")?.into(),
                Divide => builder.build_int_unsigned_div(l, r, "udiv")?.into(),
                Modulo => builder.build_int_unsigned_rem(l, r, "rem")?.into(),
            }),
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(match op {
                Add => builder.build_float_add(l, r, "fadd")?.into(),
                Subtract => builder.build_float_sub(l, r, "fsub")?.into(),
                Multiply => builder.build_float_mul(l, r, "fmul")?.into(),
                Divide => builder.build_float_div(l, r, "fdiv")?.into(),
                Modulo => builder.build_float_rem(l, r, "frem")?.into(),
            }),
            _ => panic!(
                "can't perform arithmetic on operands of types {:?} and {:?}",
                l.get_type(),
                r.get_type()
            ),
        }
    }

    pub fn build_int_cmp_binop(
        &'a self,
        op: ComparaisonOp,
        l: IntValue<'ctx>,
        r: IntValue<'ctx>,
        signed: bool,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        use ComparaisonOp::*;
        use IntPredicate::*;
        let b = &self.builder;
        Ok(match (op, signed) {
            (Equal, _) => b.build_int_compare(EQ, l, r, "eq")?,
            (NotEqual, _) => b.build_int_compare(NE, l, r, "neq")?,
            (Less, true) => b.build_int_compare(SLT, l, r, "slt")?,
            (Less, false) => b.build_int_compare(ULT, l, r, "slt")?,
            (Greater, true) => b.build_int_compare(SGT, l, r, "gt")?,
            (Greater, false) => b.build_int_compare(UGT, l, r, "gt")?,
            (LessOrEqual, true) => b.build_int_compare(SLE, l, r, "le")?,
            (LessOrEqual, false) => b.build_int_compare(ULE, l, r, "le")?,
            (GreaterOrEqual, true) => b.build_int_compare(SGE, l, r, "ge")?,
            (GreaterOrEqual, false) => b.build_int_compare(UGE, l, r, "ge")?,
        }
        .into())
    }

    pub fn build_float_cmp_binop(
        &'a self,
        op: ComparaisonOp,
        l: FloatValue<'ctx>,
        r: FloatValue<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        use ComparaisonOp::*;
        use FloatPredicate::*;
        let b = &self.builder;
        Ok(match op {
            Equal => b.build_float_compare(OEQ, l, r, "feq")?,
            NotEqual => b.build_float_compare(ONE, l, r, "fnq")?,
            Greater => b.build_float_compare(OGT, l, r, "fgt")?,
            Less => b.build_float_compare(OLT, l, r, "flt")?,
            GreaterOrEqual => b.build_float_compare(OGE, l, r, "fge")?,
            LessOrEqual => b.build_float_compare(OLE, l, r, "fle")?,
        }
        .into())
    }

    pub fn build_bitwise_binop(
        &'a self,
        op: BitwiseOp,
        l: IntValue<'ctx>,
        r: IntValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        use BitwiseOp::*;
        let builder = &self.builder;
        Ok(match op {
            And => builder.build_and(l, r, "and")?,
            Or => builder.build_or(l, r, "or")?,
            Xor => builder.build_xor(l, r, "xor")?,
            ShiftLeft => builder.build_left_shift(l, r, "lshift")?,
            ShiftRight => builder.build_right_shift(l, r, false, "rshift")?,
        })
    }

    pub fn build_logic_binop(
        &'a self,
        op: LogicOp,
        l: IntValue<'ctx>,
        r: IntValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        Ok(match op {
            LogicOp::And => self.builder.build_and(l, r, "and")?,
            LogicOp::Or => self.builder.build_or(l, r, "or")?,
        })
    }
}

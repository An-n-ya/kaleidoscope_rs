use std::ffi::CStr;

use llvm_sys::core::LLVMPrintTypeToString;
use llvm_sys::core::LLVMPrintValueToString;
use llvm_sys::prelude::LLVMTypeRef;
use llvm_sys::prelude::LLVMValueRef;

use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::Visitor;
use crate::codegen::CodeGen;

pub enum DumperKind {
    AST,
    LLVMIR(CodeGen),
}

pub struct AstDumper {
    ident: usize,
    kind: DumperKind,
}

impl AstDumper {
    pub fn new(kind: DumperKind) -> Self {
        Self { ident: 0, kind }
    }
}

pub fn llvm_type_to_string(v: LLVMTypeRef) -> String {
    let raw_str = unsafe { LLVMPrintTypeToString(v) };
    if raw_str.is_null() {
        return String::from("<error>");
    }

    let c_str = unsafe { CStr::from_ptr(raw_str) };
    c_str.to_string_lossy().into_owned()
}
pub fn llvm_value_to_string(v: LLVMValueRef) -> String {
    let raw_str = unsafe { LLVMPrintValueToString(v) };
    if raw_str.is_null() {
        return String::from("<error>");
    }

    let c_str = unsafe { CStr::from_ptr(raw_str) };
    c_str.to_string_lossy().into_owned()
}

impl Visitor<String> for AstDumper {
    fn visit_expr(&mut self, expr: &crate::ast::Expr) -> String {
        let mut res = "  ".repeat(self.ident).to_string();
        self.ident += 1;
        match &mut self.kind {
            DumperKind::AST => match expr {
                Expr::Number(number) => res.push_str(&format!("Number({})", number.val)),
                Expr::Variable(variable) => res.push_str(&format!("Variable({})", variable.name)),
                Expr::Call(call) => {
                    res.push_str(&format!("Call({})\n", call.callee));
                    for e in &call.args {
                        res.push_str(&e.accept(self));
                    }
                }
                Expr::Binary(binary) => {
                    res.push_str(&format!("Binary({:?})\n", binary.op));
                    for e in [&binary.lhs, &binary.rhs] {
                        res.push_str(&e.accept(self));
                    }
                }
            },
            DumperKind::LLVMIR(code_gen) => {
                let v = expr.accept(code_gen);
                res.push_str(&llvm_value_to_string(v));
            }
        }
        if !res.ends_with('\n') {
            res.push('\n');
        }
        self.ident -= 1;
        res
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::Stmt) -> String {
        let mut res = "  ".repeat(self.ident).to_string();
        self.ident += 1;
        match &mut self.kind {
            DumperKind::AST => match stmt {
                Stmt::Prototype(prototype) => {
                    res.push_str(&format!(
                        "Prototype: {}({})",
                        prototype.name,
                        prototype.args.join(",")
                    ));
                }
                Stmt::Function(function) => {
                    let proto = Stmt::Prototype(function.prototype.clone());
                    res.push_str("Function:\n");
                    res.push_str("prototype:\n");

                    res.push_str(&proto.accept(self));
                    res.push_str("expr:\n");
                    res.push_str(&function.expr.accept(self));
                }
                Stmt::Expr(expr_stmt) => {
                    res.push_str("ExprStmt\n");
                    res.push_str(&expr_stmt.expr.accept(self));
                }
            },
            DumperKind::LLVMIR(code_gen) => {
                let v = stmt.accept(code_gen);
                res.push_str(&llvm_value_to_string(v));
            }
        }
        if !res.ends_with('\n') {
            res.push('\n');
        }
        self.ident -= 1;
        res
    }
}

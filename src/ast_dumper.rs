use std::ffi::CStr;

use llvm_sys::core::LLVMPrintModuleToString;
use llvm_sys::core::LLVMPrintTypeToString;
use llvm_sys::core::LLVMPrintValueToString;
use llvm_sys::prelude::LLVMModuleRef;
use llvm_sys::prelude::LLVMTypeRef;
use llvm_sys::prelude::LLVMValueRef;

use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::Visitor;

pub enum DumperKind {
    AST,
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

#[allow(unused)]
pub fn llvm_type_to_string(v: LLVMTypeRef) -> String {
    let raw_str = unsafe { LLVMPrintTypeToString(v) };
    c_str_to_string(raw_str)
}
#[allow(unused)]
pub fn llvm_value_to_string(v: LLVMValueRef) -> String {
    let raw_str = unsafe { LLVMPrintValueToString(v) };
    c_str_to_string(raw_str)
}
pub fn llvm_module_to_string(v: LLVMModuleRef) -> String {
    let raw_str = unsafe { LLVMPrintModuleToString(v) };
    c_str_to_string(raw_str)
}

fn c_str_to_string(raw_str: *mut i8) -> String {
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
                Stmt::Assign(assign) => {
                    res.push_str("AssignStmt\n");
                    res.push_str(&format!("{} = ", assign.name));
                    res.push_str(&assign.expr.accept(self));
                }
            },
        }
        if !res.ends_with('\n') {
            res.push('\n');
        }
        self.ident -= 1;
        res
    }
}

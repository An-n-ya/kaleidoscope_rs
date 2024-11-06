use std::collections::HashMap;
use std::ffi::CString;
use std::ptr;

use llvm_sys::analysis::LLVMVerifyFunction;
use llvm_sys::core::LLVMAddFunction;
use llvm_sys::core::LLVMAppendBasicBlockInContext;
use llvm_sys::core::LLVMBuildCall2;
use llvm_sys::core::LLVMBuildFAdd;
use llvm_sys::core::LLVMBuildFCmp;
use llvm_sys::core::LLVMBuildFSub;
use llvm_sys::core::LLVMBuildRet;
use llvm_sys::core::LLVMBuildUIToFP;
use llvm_sys::core::LLVMConstReal;
use llvm_sys::core::LLVMContextCreate;
use llvm_sys::core::LLVMCountParams;
use llvm_sys::core::LLVMCreateBuilderInContext;
use llvm_sys::core::LLVMDeleteFunction;
use llvm_sys::core::LLVMDoubleTypeInContext;
use llvm_sys::core::LLVMFunctionType;
use llvm_sys::core::LLVMGetFirstBasicBlock;
use llvm_sys::core::LLVMGetFirstParam;
use llvm_sys::core::LLVMGetNamedFunction;
use llvm_sys::core::LLVMGetNextParam;
use llvm_sys::core::LLVMModuleCreateWithName;
use llvm_sys::core::LLVMModuleCreateWithNameInContext;
use llvm_sys::core::LLVMPositionBuilderAtEnd;
use llvm_sys::core::LLVMSetValueName2;
use llvm_sys::prelude::LLVMBuilderRef;
use llvm_sys::prelude::LLVMContextRef;
use llvm_sys::prelude::LLVMModuleRef;
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::target::LLVMSetModuleDataLayout;
use llvm_sys::LLVMRealPredicate;

use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::Visitor;

// TODO: refactor LLVM interface

#[derive(Clone)]
pub struct CodeGen {
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    module: LLVMModuleRef,
    named_values: HashMap<String, LLVMValueRef>,
}

impl CodeGen {
    pub fn new() -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let module_name = CString::new("Kaleidoscope").unwrap();
            let module = LLVMModuleCreateWithNameInContext(module_name.as_ptr(), context);
            // LLVMSetModuleDataLayout(module, R);
            let builder = LLVMCreateBuilderInContext(context);
            Self {
                context,
                module,
                builder,
                named_values: HashMap::new(),
            }
        }
    }
}

impl Visitor<LLVMValueRef> for CodeGen {
    fn visit_expr(&mut self, expr: &Expr) -> LLVMValueRef {
        unsafe {
            match expr {
                Expr::Number(number) => {
                    let double_type = LLVMDoubleTypeInContext(self.context);
                    LLVMConstReal(double_type, number.val)
                }
                Expr::Variable(variable) => {
                    let res = self.named_values.get(&variable.name);
                    if let Some(res) = res {
                        *res
                    } else {
                        panic!("Unknown variable name {}", variable.name);
                    }
                }
                Expr::Call(call) => {
                    let name = CString::new(call.callee.clone()).unwrap();
                    let callee_f = LLVMGetNamedFunction(self.module, name.as_ptr());
                    if callee_f.is_null() {
                        panic!("Unknown function referenced");
                    }

                    let expected_arg_count = LLVMCountParams(callee_f) as usize;
                    if expected_arg_count != call.args.len() {
                        panic!("Incorrect number of arguments passed");
                    }

                    let mut args = Vec::with_capacity(expected_arg_count);
                    for arg in &call.args {
                        args.push(arg.accept(self));
                    }

                    // FIXME: cannot acquire type of callee_f, this is a workaround
                    let double_type = LLVMDoubleTypeInContext(self.context);
                    let mut doubles = vec![double_type; expected_arg_count];
                    let function_type = LLVMFunctionType(
                        double_type,
                        doubles.as_mut_ptr(),
                        doubles.len() as u32,
                        0,
                    );

                    let name = CString::new("calltmp").unwrap();
                    let call = LLVMBuildCall2(
                        self.builder,
                        function_type,
                        callee_f,
                        args.as_mut_ptr(),
                        expected_arg_count as u32,
                        name.as_ptr(),
                    );

                    if call.is_null() {
                        return ptr::null_mut();
                    }
                    call
                }
                Expr::Binary(binary) => {
                    let l = binary.lhs.accept(self);
                    let r = binary.rhs.accept(self);
                    if l.is_null() || r.is_null() {
                        panic!("invalid operate");
                    }
                    match binary.op {
                        crate::token::Operator::PLUS => {
                            let name = CString::new("addtmp").unwrap();
                            LLVMBuildFAdd(self.builder, l, r, name.as_ptr())
                        }
                        crate::token::Operator::MINUS => {
                            let name = CString::new("subtmp").unwrap();
                            LLVMBuildFSub(self.builder, l, r, name.as_ptr())
                        }
                        crate::token::Operator::LESS => {
                            let name = CString::new("lesstmp").unwrap();

                            let v = LLVMBuildFCmp(
                                self.builder,
                                LLVMRealPredicate::LLVMRealULT,
                                l,
                                r,
                                name.as_ptr(),
                            );
                            let name = CString::new("booltmp").unwrap();
                            LLVMBuildUIToFP(
                                self.builder,
                                v,
                                LLVMDoubleTypeInContext(self.context),
                                name.as_ptr(),
                            )
                        }
                        crate::token::Operator::GREATER => {
                            let name = CString::new("greatertmp").unwrap();
                            let v = LLVMBuildFCmp(
                                self.builder,
                                LLVMRealPredicate::LLVMRealUGT,
                                l,
                                r,
                                name.as_ptr(),
                            );
                            let name = CString::new("booltmp").unwrap();
                            LLVMBuildUIToFP(
                                self.builder,
                                v,
                                LLVMDoubleTypeInContext(self.context),
                                name.as_ptr(),
                            )
                        }
                    }
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> LLVMValueRef {
        unsafe {
            match stmt {
                Stmt::Prototype(prototype) => {
                    let double_type = LLVMDoubleTypeInContext(self.context);
                    let mut doubles = vec![double_type; prototype.args.len()];
                    let function_type = LLVMFunctionType(
                        double_type,
                        doubles.as_mut_ptr(),
                        doubles.len() as u32,
                        0,
                    );
                    let name = CString::new(prototype.name.clone()).unwrap();
                    let func = LLVMAddFunction(self.module, name.as_ptr(), function_type);
                    if func.is_null() {
                        panic!("Error creating function");
                    }

                    let mut idx = 0;
                    let mut param_iter = LLVMGetFirstParam(func);
                    while !param_iter.is_null() && idx < prototype.args.len() {
                        let arg_name = CString::new(prototype.args[idx].as_str()).unwrap();
                        LLVMSetValueName2(param_iter, arg_name.as_ptr(), arg_name.as_bytes().len());
                        param_iter = LLVMGetNextParam(param_iter);
                        idx += 1;
                    }

                    func
                }
                Stmt::Function(function) => {
                    let name = CString::new(function.prototype.name.clone()).unwrap();
                    let mut func = LLVMGetNamedFunction(self.module, name.as_ptr());
                    if func.is_null() {
                        func = function.get_proto().accept(self);
                    }
                    if func.is_null() {
                        return ptr::null_mut();
                    }
                    if !LLVMGetFirstBasicBlock(func).is_null() {
                        panic!("Function cannot be redefined");
                    }
                    let name = CString::new("entry").unwrap();
                    let block = LLVMAppendBasicBlockInContext(self.context, func, name.as_ptr());
                    LLVMPositionBuilderAtEnd(self.builder, block);
                    self.named_values.clear();
                    let mut idx = 0;
                    let mut param_iter = LLVMGetFirstParam(func);
                    while !param_iter.is_null() && idx < function.prototype.args.len() {
                        let name = function.prototype.args[idx].clone();
                        self.named_values.insert(name, param_iter);
                        param_iter = LLVMGetNextParam(param_iter);
                        idx += 1;
                    }
                    let ret = function.expr.accept(self);
                    if ret.is_null() {
                        LLVMDeleteFunction(func);
                    }
                    LLVMBuildRet(self.builder, ret);
                    LLVMVerifyFunction(
                        func,
                        llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                    );
                    func
                }
                Stmt::Expr(expr_stmt) => expr_stmt.expr.accept(self),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

    use super::*;
    #[test]
    fn test1() -> miette::Result<()> {
        let parser = Parser {};
        let input = "4 + 5;
def foo(a,b) a + b;
def boo(a) a < foo(a,2);";
        let res = parser.parse(input)?;
        let code_gen = CodeGen::new();
        let dumped_stmts = res
            .iter()
            .map(|stmt| stmt.dump_ir(code_gen.clone()))
            .collect::<Vec<_>>();
        for s in &dumped_stmts {
            println!("{s}");
        }
        let expect = vec![
            "double 9.000000e+00\n",
            "define double @foo(double %a, double %b) {
entry:
  %addtmp = fadd double %a, %b
  ret double %addtmp
}\n",
            "define double @boo(double %a) {
entry:
  %calltmp = call double @foo(double %a, double 2.000000e+00)
  %lesstmp = fcmp ult double %a, %calltmp
  %booltmp = uitofp i1 %lesstmp to double
  ret double %booltmp
}\n",
        ];
        assert_eq!(dumped_stmts, expect);
        Ok(())
    }
}

use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::Visitor;

pub struct AstDumper {
    ident: usize,
}

impl AstDumper {
    pub fn new() -> Self {
        Self { ident: 0 }
    }
}

impl Visitor<String> for AstDumper {
    fn visit_expr(&mut self, expr: &crate::ast::Expr) -> String {
        let mut res = "  ".repeat(self.ident).to_string();
        self.ident += 1;
        match expr {
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
        match stmt {
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
        }
        if !res.ends_with('\n') {
            res.push('\n');
        }
        self.ident -= 1;
        res
    }
}

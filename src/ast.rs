use crate::token::Operator;

pub enum Expr {
    Number(Number),
    Variable(Variable),
    Call(Call),
    Binary(Binary),
}
pub enum Stmt {
    Prototype(Prototype),
    Function(Function),
    Expr(ExprStmt),
}

pub struct Number {
    val: f64,
}
impl Number {
    pub fn new(val: f64) -> Expr {
        Expr::Number(Self { val })
    }
}

pub struct Variable {
    name: String,
}
impl Variable {
    pub fn new(name: String) -> Expr {
        Expr::Variable(Self { name })
    }
}

pub struct Binary {
    op: Operator,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}
impl Binary {
    pub fn new(op: Operator, lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
        Expr::Binary(Self { op, lhs, rhs })
    }
}

pub struct Call {
    callee: String,
    args: Vec<Expr>,
}
impl Call {
    pub fn new(callee: String, args: Vec<Expr>) -> Expr {
        Expr::Call(Self { callee, args })
    }
}

#[derive(Clone)]
pub struct Prototype {
    name: String,
    args: Vec<String>,
}
impl Prototype {
    pub fn new(name: String, args: Vec<String>) -> Stmt {
        Stmt::Prototype(Self { name, args })
    }
}

pub struct Function {
    prototype: Prototype,
    expr: Expr,
}
impl Function {
    pub fn new(prototype: Stmt, expr: Expr) -> Stmt {
        match prototype {
            Stmt::Prototype(prototype) => Stmt::Function(Self { prototype, expr }),
            _ => {
                panic!("should be prototype")
            }
        }
    }
}
pub struct ExprStmt {
    expr: Expr,
}
impl ExprStmt {
    pub fn new(expr: Expr) -> Stmt {
        Stmt::Expr(Self { expr })
    }
}

impl Expr {
    pub fn dump(&self) -> String {
        self.dump_with_ident(0)
    }

    fn dump_with_ident(&self, ident: usize) -> String {
        let mut res = "  ".repeat(ident).to_string();
        match self {
            Expr::Number(number) => res.push_str(&format!("Number({})", number.val)),
            Expr::Variable(variable) => res.push_str(&format!("Variable({})", variable.name)),
            Expr::Call(call) => {
                res.push_str(&format!("Call({})\n", call.callee));
                for e in &call.args {
                    res.push_str(&e.dump_with_ident(ident + 1));
                }
            }
            Expr::Binary(binary) => {
                res.push_str(&format!("Binary({:?})\n", binary.op));
                for e in [&binary.lhs, &binary.rhs] {
                    res.push_str(&e.dump_with_ident(ident + 1));
                }
            }
        }
        if !res.ends_with('\n') {
            res.push('\n');
        }
        res
    }
}

impl Stmt {
    pub fn dump(&self) -> String {
        self.dump_with_ident(0)
    }
    fn dump_with_ident(&self, ident: usize) -> String {
        let mut res = "  ".repeat(ident).to_string();
        match self {
            Stmt::Prototype(prototype) => {
                res.push_str(&format!(
                    "Prototype: {}({})",
                    prototype.name,
                    prototype.args.join(",")
                ));
            }
            Stmt::Function(function) => {
                let proto = Stmt::Prototype(function.prototype.clone());
                res.push_str("prototype:\n");
                res.push_str(&proto.dump_with_ident(ident + 1));
                res.push_str("expr:\n");
                res.push_str(&function.expr.dump_with_ident(ident + 1));
            }
            Stmt::Expr(expr_stmt) => {
                res.push_str("ExprStmt\n");
                res.push_str(&expr_stmt.expr.dump_with_ident(ident + 1));
            }
        }
        if !res.ends_with('\n') {
            res.push('\n');
        }
        res
    }
}

use crate::{ast_dumper::AstDumper, token::Operator};

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

pub trait Visitor<R> {
    fn visit_expr(&mut self, expr: &Expr) -> R;
    fn visit_stmt(&mut self, stmt: &Stmt) -> R;
}

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
        visitor.visit_expr(self)
    }
}
impl Stmt {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
        visitor.visit_stmt(self)
    }
}

pub struct Number {
    pub val: f64,
}
impl Number {
    pub fn new(val: f64) -> Expr {
        Expr::Number(Self { val })
    }
}

pub struct Variable {
    pub name: String,
}
impl Variable {
    pub fn new(name: String) -> Expr {
        Expr::Variable(Self { name })
    }
}

pub struct Binary {
    pub op: Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}
impl Binary {
    pub fn new(op: Operator, lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
        Expr::Binary(Self { op, lhs, rhs })
    }
}

pub struct Call {
    pub callee: String,
    pub args: Vec<Expr>,
}
impl Call {
    pub fn new(callee: String, args: Vec<Expr>) -> Expr {
        Expr::Call(Self { callee, args })
    }
}

#[derive(Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}
impl Prototype {
    pub fn new(name: String, args: Vec<String>) -> Stmt {
        Stmt::Prototype(Self { name, args })
    }
}

pub struct Function {
    pub prototype: Prototype,
    pub expr: Expr,
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
    pub expr: Expr,
}
impl ExprStmt {
    pub fn new(expr: Expr) -> Stmt {
        Stmt::Expr(Self { expr })
    }
}

impl Expr {
    pub fn dump(&self) -> String {
        let mut dumper = AstDumper::new();
        self.accept(&mut dumper)
    }
}

impl Stmt {
    pub fn dump(&self) -> String {
        let mut dumper = AstDumper::new();
        self.accept(&mut dumper)
    }
}

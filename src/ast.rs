use crate::{ast_dumper::AstDumper, token::Operator};

#[derive(Debug)]
pub enum Expr {
    Number(Number),
    Variable(Variable),
    Call(Call),
    Binary(Binary),
}
#[derive(Debug)]
pub enum Stmt {
    Prototype(Prototype),
    Function(Function),
    Expr(ExprStmt),
    Assign(AssignStmt),
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

#[derive(Debug)]
pub struct Number {
    pub val: f64,
}
impl Number {
    pub fn new(val: f64) -> Expr {
        Expr::Number(Self { val })
    }
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
}
impl Variable {
    pub fn new(name: String) -> Expr {
        Expr::Variable(Self { name })
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Call {
    pub callee: String,
    pub args: Vec<Expr>,
}
impl Call {
    pub fn new(callee: String, args: Vec<Expr>) -> Expr {
        Expr::Call(Self { callee, args })
    }
}

#[derive(Clone, Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
    pub _extern: bool,
}
impl Prototype {
    pub fn new(name: String, args: Vec<String>, _extern: bool) -> Stmt {
        Stmt::Prototype(Self {
            name,
            args,
            _extern,
        })
    }
}

#[derive(Debug)]
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
    pub fn get_proto(&self) -> Stmt {
        Stmt::Prototype(self.prototype.clone())
    }
}
#[derive(Debug)]
pub struct AssignStmt {
    pub name: String,
    pub expr: Expr,
}
impl AssignStmt {
    pub fn new(name: String, expr: Expr) -> Stmt {
        Stmt::Assign(Self { name, expr })
    }
}
#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}
impl ExprStmt {
    pub fn new(expr: Expr) -> Stmt {
        Stmt::Expr(Self { expr })
    }
}

#[allow(unused)]
impl Expr {
    pub fn dump(&self) -> String {
        let mut dumper = AstDumper::new(crate::ast_dumper::DumperKind::AST);
        self.accept(&mut dumper)
    }
}

impl Stmt {
    pub fn dump(&self) -> String {
        let mut dumper = AstDumper::new(crate::ast_dumper::DumperKind::AST);
        self.accept(&mut dumper)
    }
}

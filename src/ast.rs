use crate::token::{Operator, Token};

pub trait ASTNode {
    fn to_proto(&self) -> Option<Prototype> {
        None
    }
}
pub trait ExprAST: ASTNode {}
pub trait StmtAST: ASTNode {}
pub type Expr = Box<dyn ExprAST>;
pub type Stmt = Box<dyn StmtAST>;
pub type AST = Box<dyn ASTNode>;

pub struct NumberExprAST {
    val: f64,
}
impl ASTNode for NumberExprAST {}
impl ExprAST for NumberExprAST {}
impl NumberExprAST {
    pub fn new(val: f64) -> Expr {
        Box::new(Self { val })
    }
}

pub struct VariableExprAST {
    name: String,
}
impl ASTNode for VariableExprAST {}
impl ExprAST for VariableExprAST {}
impl VariableExprAST {
    pub fn new(name: String) -> Expr {
        Box::new(Self { name })
    }
}

pub struct BinaryExprAST {
    op: Operator,
    lhs: Box<dyn ExprAST>,
    rhs: Box<dyn ExprAST>,
}
impl ASTNode for BinaryExprAST {}
impl ExprAST for BinaryExprAST {}
impl BinaryExprAST {
    pub fn new(op: Operator, lhs: Box<dyn ExprAST>, rhs: Box<dyn ExprAST>) -> Expr {
        Box::new(Self { op, lhs, rhs })
    }
}

pub struct CallExprAST {
    callee: String,
    args: Vec<Box<dyn ExprAST>>,
}
impl ASTNode for CallExprAST {}
impl ExprAST for CallExprAST {}
impl CallExprAST {
    pub fn new(callee: String, args: Vec<Box<dyn ExprAST>>) -> Expr {
        Box::new(Self { callee, args })
    }
}

#[derive(Clone)]
pub struct Prototype {
    name: String,
    args: Vec<String>,
}
impl ASTNode for Prototype {
    fn to_proto(&self) -> Option<Prototype> {
        Some(self.clone())
    }
}
impl StmtAST for Prototype {}
impl Prototype {
    pub fn new(name: String, args: Vec<String>) -> Stmt {
        Box::new(Self { name, args })
    }
}

pub struct Function {
    prototype: Prototype,
    expr: Expr,
}
impl ASTNode for Function {}
impl StmtAST for Function {}
impl Function {
    pub fn new(prototype: Prototype, expr: Expr) -> Stmt {
        Box::new(Self { prototype, expr })
    }
}
pub struct ExprStmt {
    expr: Expr,
}
impl ASTNode for ExprStmt {}
impl StmtAST for ExprStmt {}
impl ExprStmt {
    pub fn new(expr: Expr) -> Stmt {
        Box::new(Self { expr })
    }
}

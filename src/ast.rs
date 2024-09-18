use crate::token::{Operator, Token};

pub trait ExprAST {}
pub type Expr = Box<dyn ExprAST>;

pub struct NumberExprAST {
    val: f64,
}
impl ExprAST for NumberExprAST {}
impl NumberExprAST {
    pub fn new(val: f64) -> Expr {
        Box::new(Self { val })
    }
}

pub struct VariableExprAST {
    name: String,
}
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
impl ExprAST for CallExprAST {}
impl CallExprAST {
    pub fn new(callee: String, args: Vec<Box<dyn ExprAST>>) -> Expr {
        Box::new(Self { callee, args })
    }
}

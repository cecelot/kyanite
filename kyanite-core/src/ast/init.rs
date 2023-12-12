use std::rc::Rc;

use crate::token::Token;

use super::{node, Decl, Expr, Field, Initializer, Param, Stmt};

pub fn record(name: Token, fields: Vec<Field>) -> Decl {
    Decl::Record(Rc::new(node::RecordDecl::new(name, fields)))
}

pub fn init(name: Token, initializers: Vec<Initializer>, parens: (Token, Token)) -> Expr {
    Expr::Init(Rc::new(node::Init::new(name, initializers, parens)))
}

pub fn access(accesses: Vec<Expr>) -> Expr {
    Expr::Access(Rc::new(node::Access::new(accesses)))
}

pub fn conditional(condition: Expr, is: Vec<Stmt>, otherwise: Vec<Stmt>) -> Stmt {
    Stmt::If(Rc::new(node::If::new(condition, is, otherwise)))
}

pub fn loops(condition: Expr, body: Vec<Stmt>) -> Stmt {
    Stmt::While(Rc::new(node::While::new(condition, body)))
}

pub fn func(
    name: Token,
    params: Vec<Param>,
    ty: Option<Token>,
    body: Vec<Stmt>,
    external: bool,
) -> Decl {
    Decl::Function(Rc::new(node::FuncDecl::new(
        name, params, ty, body, external,
    )))
}

pub fn assign(target: Expr, expr: Expr) -> Stmt {
    Stmt::Assign(Rc::new(node::Assign::new(target, expr)))
}

pub fn var(name: Token, ty: Token, init: Expr) -> Stmt {
    Stmt::Var(Rc::new(node::VarDecl::new(name, ty, init)))
}

pub fn constant(name: Token, ty: Token, init: Expr) -> Decl {
    Decl::Constant(Rc::new(node::ConstantDecl::new(name, ty, init)))
}

pub fn call(left: Expr, args: Vec<Expr>, parens: (Token, Token), delimiters: Vec<Token>) -> Expr {
    Expr::Call(Rc::new(node::Call::new(
        Box::new(left),
        args,
        parens,
        delimiters,
    )))
}

pub fn ret(expr: Expr, token: Token) -> Stmt {
    Stmt::Return(Rc::new(node::Return::new(expr, token)))
}

pub fn unary(op: Token, expr: Expr) -> Expr {
    Expr::Unary(Rc::new(node::Unary::new(op, Box::new(expr))))
}

pub fn binary(left: Expr, op: Token, right: Expr) -> Expr {
    Expr::Binary(Rc::new(node::Binary::new(
        Box::new(left),
        op,
        Box::new(right),
    )))
}

pub fn ident(name: Token) -> Expr {
    Expr::Ident(Rc::new(node::Ident::new(name)))
}

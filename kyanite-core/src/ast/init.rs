use crate::token::Token;

use super::{node, Decl, Expr, Field, Initializer, Param, Stmt};

pub fn record(name: Token, fields: Vec<Field>) -> Decl {
    Decl::Record(node::RecordDecl::new(name, fields))
}

pub fn init(name: Token, initializers: Vec<Initializer>) -> Expr {
    Expr::Init(node::Init::new(name, initializers))
}

pub fn func(
    name: Token,
    params: Vec<Param>,
    ty: Option<Token>,
    body: Vec<Stmt>,
    external: bool,
) -> Decl {
    Decl::Function(node::FuncDecl::new(name, params, ty, body, external))
}

pub fn assign(target: Expr, expr: Expr) -> Stmt {
    Stmt::Assign(node::Assign::new(target, expr))
}

pub fn var(name: Token, ty: Token, init: Expr) -> Stmt {
    Stmt::Var(node::VarDecl::new(name, ty, init))
}

pub fn constant(name: Token, ty: Token, init: Expr) -> Decl {
    Decl::Constant(node::ConstantDecl::new(name, ty, init))
}

pub fn call(left: Expr, args: Vec<Expr>, parens: (Token, Token), delimiters: Vec<Token>) -> Expr {
    Expr::Call(node::Call::new(Box::new(left), args, parens, delimiters))
}

pub fn ret(expr: Expr, token: Token) -> Stmt {
    Stmt::Return(node::Return::new(expr, token))
}

pub fn unary(op: Token, right: Expr) -> Expr {
    Expr::Unary(node::Unary::new(op, Box::new(right)))
}

pub fn binary(left: Expr, op: Token, right: Expr) -> Expr {
    Expr::Binary(node::Binary::new(Box::new(left), op, Box::new(right)))
}

pub fn ident(name: Token) -> Expr {
    Expr::Ident(node::Ident::new(name))
}

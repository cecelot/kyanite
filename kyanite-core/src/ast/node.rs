use std::sync::atomic::{AtomicUsize, Ordering};

use serde::{Deserialize, Serialize};

use crate::token::Token;

use super::{Expr, Field, Initializer, Param, Stmt};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RecordDecl {
    pub name: Token,
    pub fields: Vec<Field>,
}

impl RecordDecl {
    pub fn new(name: Token, fields: Vec<Field>) -> Self {
        Self { name, fields }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuncDecl {
    pub name: Token,
    pub params: Vec<Param>,
    pub ty: Option<Token>,
    pub body: Vec<Stmt>,
    pub external: bool,
}

impl FuncDecl {
    pub fn new(
        name: Token,
        params: Vec<Param>,
        ty: Option<Token>,
        body: Vec<Stmt>,
        external: bool,
    ) -> Self {
        Self {
            name,
            params,
            ty,
            body,
            external,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assign {
    pub target: Expr,
    pub expr: Expr,
}

impl Assign {
    pub fn new(target: Expr, expr: Expr) -> Self {
        Self { target, expr }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarDecl {
    pub name: Token,
    pub ty: Token,
    pub expr: Expr,
}

impl VarDecl {
    pub fn new(name: Token, ty: Token, expr: Expr) -> Self {
        Self { name, ty, expr }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstantDecl {
    pub name: Token,
    pub ty: Token,
    pub expr: Expr,
}

impl ConstantDecl {
    pub fn new(name: Token, ty: Token, expr: Expr) -> Self {
        Self { name, ty, expr }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Init {
    pub name: Token,
    pub initializers: Vec<Initializer>,
    pub parens: (Token, Token),
}

impl Init {
    pub fn new(name: Token, initializers: Vec<Initializer>, parens: (Token, Token)) -> Self {
        Self {
            name,
            initializers,
            parens,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Call {
    pub left: Box<Expr>,
    pub args: Vec<Expr>,
    pub parens: (Token, Token),
    pub delimiters: Vec<Token>,
}

impl Call {
    pub fn new(
        left: Box<Expr>,
        args: Vec<Expr>,
        parens: (Token, Token),
        delimiters: Vec<Token>,
    ) -> Self {
        Self {
            left,
            args,
            parens,
            delimiters,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Access {
    pub chain: Vec<Expr>,
    #[serde(skip)]
    pub id: usize,
}

impl Access {
    pub fn new(chain: Vec<Expr>) -> Self {
        static ID: AtomicUsize = AtomicUsize::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        Self { chain, id }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Return {
    pub expr: Expr,
    pub keyword: Token,
}

impl Return {
    pub fn new(expr: Expr, keyword: Token) -> Self {
        Self { expr, keyword }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Unary {
    pub op: Token,
    pub right: Box<Expr>,
}

impl Unary {
    pub fn new(op: Token, right: Box<Expr>) -> Self {
        Self { op, right }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Binary {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

impl Binary {
    pub fn new(left: Box<Expr>, op: Token, right: Box<Expr>) -> Self {
        Self { left, op, right }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ident {
    pub name: Token,
}

impl Ident {
    pub fn new(name: Token) -> Self {
        Self { name }
    }
}

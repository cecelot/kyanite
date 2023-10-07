use serde::{Deserialize, Serialize};
use std::fmt;

use crate::{ast::Type, token::Token};

use super::{Expr, Param, Stmt};

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

impl fmt::Display for FuncDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "defn {}(", String::from(&self.name))?;
        for (i, param) in self.params.iter().enumerate() {
            write!(
                f,
                "{}: {:?}",
                String::from(&param.name),
                Type::from(&param.ty)
            )?;
            if i < self.params.len() - 1 {
                write!(f, ", ")?;
            }
        }
        writeln!(f, "): {:?} {{", Type::from(self.ty.as_ref()))?;
        for stmt in &self.body {
            writeln!(f, "\t{}", stmt)?;
        }
        write!(f, "}}")
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

impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {};", self.target, self.expr)
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

impl fmt::Display for VarDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "let {}: {:?} = {};",
            String::from(&self.name),
            Type::from(&self.ty),
            self.expr
        )
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

impl fmt::Display for ConstantDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "const {}: {:?} = {};",
            String::from(&self.name),
            Type::from(&self.ty),
            self.expr
        )
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

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.left)?;
        for (i, arg) in self.args.iter().enumerate() {
            write!(f, "{}", arg)?;
            if i < self.args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
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

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.expr)
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

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.op, self.right)
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

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
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

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from(&self.name))
    }
}

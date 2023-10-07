use serde::{Deserialize, Serialize};
use std::fmt;

use crate::{ast::Type, token::Token};

use super::{Node, Param};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuncDecl {
    pub name: Token,
    pub params: Vec<Param>,
    pub ty: Option<Token>,
    pub body: Vec<Node>,
    pub external: bool,
}

impl FuncDecl {
    pub fn new(
        name: Token,
        params: Vec<Param>,
        ty: Option<Token>,
        body: Vec<Node>,
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
    pub target: Box<Node>,
    pub expr: Box<Node>,
}

impl Assign {
    pub fn new(target: Box<Node>, expr: Box<Node>) -> Self {
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
    pub expr: Box<Node>,
}

impl VarDecl {
    pub fn new(name: Token, ty: Token, expr: Box<Node>) -> Self {
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
    pub expr: Box<Node>,
}

impl ConstantDecl {
    pub fn new(name: Token, ty: Token, expr: Box<Node>) -> Self {
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
    pub left: Box<Node>,
    pub args: Vec<Node>,
    pub parens: (Token, Token),
    pub delimiters: Vec<Token>,
}

impl Call {
    pub fn new(
        left: Box<Node>,
        args: Vec<Node>,
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
    pub expr: Box<Node>,
    pub keyword: Token,
}

impl Return {
    pub fn new(expr: Box<Node>, keyword: Token) -> Self {
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
    pub right: Box<Node>,
}

impl Unary {
    pub fn new(op: Token, right: Box<Node>) -> Self {
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
    pub left: Box<Node>,
    pub op: Token,
    pub right: Box<Node>,
}

impl Binary {
    pub fn new(left: Box<Node>, op: Token, right: Box<Node>) -> Self {
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

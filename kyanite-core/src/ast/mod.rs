use inkwell::{types::BasicTypeEnum, AddressSpace};

use crate::{
    codegen::Ir,
    parse::{ParseError, Parser},
    token::Token,
};
use std::fmt;

pub mod node;

#[derive(Debug)]
pub struct Ast {
    pub file: File,
}

impl Ast {
    pub fn new(tokens: Vec<Token>) -> Result<Self, ParseError> {
        Ok(Self {
            file: Parser::from(tokens).parse()?,
        })
    }
}

impl From<Vec<Token>> for Ast {
    fn from(tokens: Vec<Token>) -> Self {
        match Ast::new(tokens) {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    }
}

#[derive(Debug)]
pub struct File {
    pub nodes: Vec<Node>,
}

impl File {
    pub fn new(nodes: Vec<Node>) -> Self {
        Self { nodes }
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in &self.nodes {
            writeln!(f, "{}", item)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    FuncDecl(node::FuncDecl),
    Assign(node::Assign),
    ConstantDecl(node::ConstantDecl),
    VarDecl(node::VarDecl),
    Call(node::Call),
    Return(node::Return),
    Binary(node::Binary),
    Unary(node::Unary),
    Ident(node::Ident),
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    #[allow(dead_code)]
    Void,
}

impl Node {
    pub fn func(name: Token, params: Vec<Param>, ty: Option<Token>, body: Vec<Node>) -> Self {
        Self::FuncDecl(node::FuncDecl::new(name, params, ty, body))
    }

    pub fn assign(target: Node, expr: Node) -> Self {
        Self::Assign(node::Assign::new(Box::new(target), Box::new(expr)))
    }

    pub fn var(name: Token, ty: Token, init: Node) -> Self {
        Self::VarDecl(node::VarDecl::new(name, ty, Box::new(init)))
    }

    pub fn constant(name: Token, ty: Token, init: Node) -> Self {
        Self::ConstantDecl(node::ConstantDecl::new(name, ty, Box::new(init)))
    }

    pub fn call(
        left: Node,
        args: Vec<Node>,
        parens: (Token, Token),
        delimiters: Vec<Token>,
    ) -> Self {
        Self::Call(node::Call::new(Box::new(left), args, parens, delimiters))
    }

    pub fn ret(expr: Node, token: Token) -> Self {
        Self::Return(node::Return::new(Box::new(expr), token))
    }

    pub fn unary(op: Token, right: Node) -> Self {
        Self::Unary(node::Unary::new(op, Box::new(right)))
    }

    pub fn binary(left: Node, op: Token, right: Node) -> Self {
        Self::Binary(node::Binary::new(Box::new(left), op, Box::new(right)))
    }

    pub fn ident(name: Token) -> Self {
        Self::Ident(node::Ident::new(name))
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::FuncDecl(func) => write!(f, "{}", func),
            Node::Assign(assign) => write!(f, "{}", assign),
            Node::VarDecl(var) => write!(f, "{}", var),
            Node::ConstantDecl(constant) => write!(f, "{}", constant),
            Node::Return(ret) => write!(f, "{}", ret),
            Node::Binary(binary) => write!(f, "{}", binary),
            Node::Unary(unary) => write!(f, "{}", unary),
            Node::Call(call) => write!(f, "{}", call),
            Node::Ident(id) => write!(f, "{}", id),
            Node::Float(n) => write!(f, "{}", n),
            Node::Int(i) => write!(f, "{}", i),
            Node::Str(s) => write!(f, "{}", s),
            Node::Bool(b) => write!(f, "{}", b),
            Node::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Str,
    Int,
    Float,
    Bool,
    Void,
}

impl Type {
    pub fn llvm<'a, 'ctx>(&'a self, ir: &Ir<'a, 'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            Type::Int => ir.context.i64_type().into(),
            Type::Float => ir.context.f64_type().into(),
            Type::Str => ir
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            Type::Bool => ir.context.bool_type().into(),
            Type::Void => unimplemented!(),
        }
    }
}

impl From<&Token> for Type {
    fn from(value: &Token) -> Self {
        match value.lexeme.clone().unwrap_or("".into()).as_str() {
            "str" => Self::Str,
            "int" => Self::Int,
            "float" => Self::Float,
            "bool" => Self::Bool,
            "void" => Self::Void,
            _ => unimplemented!(),
        }
    }
}

impl From<Option<&Token>> for Type {
    fn from(token: Option<&Token>) -> Self {
        match token {
            Some(token) => Self::from(token),
            None => Self::Void,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Token,
    pub ty: Token,
}

impl Param {
    pub fn new(name: Token, ty: Token) -> Self {
        Self { name, ty }
    }
}

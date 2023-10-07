use inkwell::{types::BasicTypeEnum, AddressSpace};
use serde::{Deserialize, Serialize};

use crate::{
    codegen::Ir,
    parse::Parser,
    token::{Span, Token, TokenStream},
    PipelineError, Source,
};
use std::fmt;

pub mod node;

#[derive(Debug, Serialize, Deserialize)]
pub struct Ast {
    pub file: File,
}

impl Ast {
    pub fn from_source(source: Source) -> Result<Self, PipelineError> {
        let stream = TokenStream::from_source(source).map_err(|_| PipelineError::InvalidUtf8)?;
        Self::new(stream)
    }

    pub fn from_string(source: String) -> Result<Self, PipelineError> {
        let stream = TokenStream::from_string(source).map_err(|_| PipelineError::InvalidUtf8)?;
        Self::new(stream)
    }

    fn new(stream: TokenStream) -> Result<Self, PipelineError> {
        let errors = stream.errors.len();
        if errors > 0 {
            return Err(PipelineError::LexError(errors));
        }
        let mut parser = Parser::new(stream.source, stream.tokens);
        let file = parser.parse();
        let errors = parser.errors.len();
        if errors > 0 {
            return Err(PipelineError::ParseError(errors));
        }
        Ok(Self { file })
    }
}

#[derive(Debug, Serialize, Deserialize)]
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

pub trait NodeSpan {
    fn span(&self) -> Span {
        Span::new(self.line(), self.start(), self.end() - self.start())
    }
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn line(&self) -> usize;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
    Str(String, Token),
    Int(i64, Token),
    Float(f64, Token),
    Bool(bool, Token),
    Void,
}

impl NodeSpan for Node {
    fn start(&self) -> usize {
        match self {
            Node::Assign(assign) => assign.expr.start(),
            Node::VarDecl(var) => var.expr.start(),
            Node::ConstantDecl(constant) => constant.expr.start(),
            Node::Call(call) => call.left.start(),
            Node::Return(ret) => ret.expr.start(),
            Node::Binary(binary) => binary.left.start(),
            Node::Unary(unary) => unary.op.span.column,
            Node::Ident(id) => id.name.span.column,
            Node::FuncDecl(func) => func.name.span.column,
            Node::Str(_, token) => token.span.column,
            Node::Int(_, token) => token.span.column,
            Node::Float(_, token) => token.span.column,
            Node::Bool(_, token) => token.span.column,
            Node::Void => unimplemented!(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Node::Assign(assign) => assign.expr.end(),
            Node::VarDecl(var) => var.expr.end(),
            Node::ConstantDecl(constant) => constant.expr.end(),
            Node::Call(call) => call.parens.1.span.column + 1,
            Node::Return(ret) => ret.expr.end(),
            Node::Binary(binary) => binary.right.end(),
            Node::Unary(unary) => unary.right.end(),
            Node::Ident(id) => id.name.span.column + id.name.span.length,
            Node::FuncDecl(func) => func.name.span.column + func.name.span.length,
            Node::Str(_, token) => token.span.column + token.span.length,
            Node::Int(_, token) => token.span.column + token.span.length,
            Node::Float(_, token) => token.span.column + token.span.length,
            Node::Bool(_, token) => token.span.column + token.span.length,
            Node::Void => unimplemented!(),
        }
    }

    fn line(&self) -> usize {
        match self {
            Node::Assign(assign) => assign.expr.line(),
            Node::VarDecl(var) => var.expr.line(),
            Node::ConstantDecl(constant) => constant.expr.line(),
            Node::Call(call) => call.left.line(),
            Node::Return(ret) => ret.expr.line(),
            Node::Binary(binary) => binary.left.line(),
            Node::Unary(unary) => unary.right.line(),
            Node::Ident(id) => id.name.span.line,
            Node::FuncDecl(func) => func.name.span.line,
            Node::Str(_, token) => token.span.line,
            Node::Int(_, token) => token.span.line,
            Node::Float(_, token) => token.span.line,
            Node::Bool(_, token) => token.span.line,
            Node::Void => unimplemented!(),
        }
    }
}

impl Node {
    pub fn ty(&self) -> Type {
        match self {
            Node::FuncDecl(func) => {
                if let Some(ty) = &func.ty {
                    Type::from(ty)
                } else {
                    Type::Void
                }
            }
            Node::ConstantDecl(c) => Type::from(&c.ty),
            Node::VarDecl(v) => Type::from(&v.ty),
            Node::Str(..) => Type::Str,
            Node::Int(..) => Type::Int,
            Node::Float(..) => Type::Float,
            Node::Bool(..) => Type::Bool,
            Node::Void => Type::Void,
            Node::Assign(assign) => assign.expr.ty(),
            Node::Return(ret) => ret.expr.ty(),
            Node::Binary(binary) => binary.left.ty(),
            Node::Unary(unary) => unary.right.ty(),
            Node::Call(call) => call.left.ty(),
            Node::Ident(_) => unimplemented!(""),
        }
    }

    pub fn func(
        name: Token,
        params: Vec<Param>,
        ty: Option<Token>,
        body: Vec<Node>,
        external: bool,
    ) -> Self {
        Self::FuncDecl(node::FuncDecl::new(name, params, ty, body, external))
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
            Node::Float(n, _) => write!(f, "{}", n),
            Node::Int(i, _) => write!(f, "{}", i),
            Node::Str(s, _) => write!(f, "{}", s),
            Node::Bool(b, _) => write!(f, "{}", b),
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
    pub fn as_llvm_basic_type<'a, 'ctx>(&'a self, ir: &Ir<'a, 'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            Type::Int => ir.context.i64_type().into(),
            Type::Float => ir.context.f64_type().into(),
            Type::Str => ir
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            Type::Bool => ir.context.bool_type().into(),
            Type::Void => unimplemented!("void does not implement `BasicTypeEnum`"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = match self {
            Type::Str => "str",
            Type::Int => "int",
            Type::Float => "float",
            Type::Bool => "bool",
            Type::Void => "void",
        };
        write!(f, "{}", ty)
    }
}

impl From<&Token> for Type {
    fn from(value: &Token) -> Self {
        match value
            .lexeme
            .clone()
            .expect("token should have lexeme")
            .as_str()
        {
            "str" => Self::Str,
            "int" => Self::Int,
            "float" => Self::Float,
            "bool" => Self::Bool,
            "void" => Self::Void,
            _ => unreachable!("type lexeme must be one of `str`, `int`, `float`, `bool`, `void`"),
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Param {
    pub name: Token,
    pub ty: Token,
}

impl Param {
    pub fn new(name: Token, ty: Token) -> Self {
        Self { name, ty }
    }
}

macro_rules! assert_ast {
    ($($path:expr => $name:ident),*) => {
        #[cfg(test)]
        mod tests {
            use crate::{Source, ast};

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let ast = ast::Ast::from_source(Source::new($path)?)?;
                    insta::with_settings!({snapshot_path => "../../snapshots"}, {
                        insta::assert_yaml_snapshot!(ast);
                    });

                    Ok(())
                }
            )*
        }
    };
}

assert_ast!(
    "test-cases/hello.kya" => hello_world,
    "test-cases/expr.kya" => expr,
    "test-cases/calls.kya" => calls,
    "test-cases/empty.kya" => empty,
    "test-cases/access.kya" => access,
    "test-cases/mixed.kya" => mixed
);

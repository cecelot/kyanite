use inkwell::{types::BasicTypeEnum, AddressSpace};
use serde::{Deserialize, Serialize};

use crate::{
    codegen::Ir,
    parse::Parser,
    token::{Span, Token, TokenStream},
    PipelineError, Source,
};
use std::fmt;

pub mod init;
pub mod node;

#[derive(Debug, Serialize, Deserialize)]
pub struct Ast {
    pub nodes: Vec<Decl>,
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
        let nodes = parser.parse();
        let errors = parser.errors.len();
        if errors > 0 {
            return Err(PipelineError::ParseError(errors));
        }
        Ok(Self { nodes })
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
pub enum Decl {
    Function(node::FuncDecl),
    Constant(node::ConstantDecl),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Var(node::VarDecl),
    Assign(node::Assign),
    Return(node::Return),
    Expr(Expr),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    Call(node::Call),
    Binary(node::Binary),
    Unary(node::Unary),
    Ident(node::Ident),
    Str(String, Token),
    Int(i64, Token),
    Float(f64, Token),
    Bool(bool, Token),
}

impl NodeSpan for Decl {
    fn start(&self) -> usize {
        match self {
            Decl::Function(func) => func.name.span.column,
            Decl::Constant(constant) => constant.expr.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Decl::Function(func) => func.name.span.column + func.name.span.length,
            Decl::Constant(constant) => constant.expr.end(),
        }
    }

    fn line(&self) -> usize {
        match self {
            Decl::Function(func) => func.name.span.line,
            Decl::Constant(constant) => constant.expr.line(),
        }
    }
}

impl NodeSpan for Stmt {
    fn start(&self) -> usize {
        match self {
            Stmt::Var(var) => var.name.span.column,
            Stmt::Assign(assign) => assign.target.start(),
            Stmt::Return(ret) => ret.keyword.span.column,
            Stmt::Expr(expr) => expr.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Stmt::Var(var) => var.expr.end(),
            Stmt::Assign(assign) => assign.expr.end(),
            Stmt::Return(ret) => ret.expr.end(),
            Stmt::Expr(expr) => expr.end(),
        }
    }

    fn line(&self) -> usize {
        match self {
            Stmt::Var(var) => var.name.span.line,
            Stmt::Assign(assign) => assign.target.line(),
            Stmt::Return(ret) => ret.expr.line(),
            Stmt::Expr(expr) => expr.line(),
        }
    }
}

impl NodeSpan for Expr {
    fn start(&self) -> usize {
        match self {
            Expr::Call(call) => call.left.start(),
            Expr::Binary(binary) => binary.left.start(),
            Expr::Unary(unary) => unary.op.span.column,
            Expr::Ident(id) => id.name.span.column,
            Expr::Str(_, token) => token.span.column,
            Expr::Int(_, token) => token.span.column,
            Expr::Float(_, token) => token.span.column,
            Expr::Bool(_, token) => token.span.column,
        }
    }

    fn end(&self) -> usize {
        match self {
            Expr::Call(call) => call.parens.1.span.column + 1,
            Expr::Binary(binary) => binary.right.end(),
            Expr::Unary(unary) => unary.right.end(),
            Expr::Ident(id) => id.name.span.column + id.name.span.length,
            Expr::Str(_, token) => token.span.column + token.span.length,
            Expr::Int(_, token) => token.span.column + token.span.length,
            Expr::Float(_, token) => token.span.column + token.span.length,
            Expr::Bool(_, token) => token.span.column + token.span.length,
        }
    }

    fn line(&self) -> usize {
        match self {
            Expr::Call(call) => call.left.line(),
            Expr::Binary(binary) => binary.left.line(),
            Expr::Unary(unary) => unary.right.line(),
            Expr::Ident(id) => id.name.span.line,
            Expr::Str(_, token) => token.span.line,
            Expr::Int(_, token) => token.span.line,
            Expr::Float(_, token) => token.span.line,
            Expr::Bool(_, token) => token.span.line,
        }
    }
}

impl Expr {
    pub fn ty(&self) -> Type {
        match self {
            Expr::Str(..) => Type::Str,
            Expr::Int(..) => Type::Int,
            Expr::Float(..) => Type::Float,
            Expr::Bool(..) => Type::Bool,
            Expr::Binary(binary) => binary.left.ty(),
            Expr::Unary(unary) => unary.right.ty(),
            Expr::Call(call) => call.left.ty(),
            Expr::Ident(_) => unimplemented!(""),
        }
    }
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Decl::Function(func) => write!(f, "{}", func),
            Decl::Constant(constant) => write!(f, "{}", constant),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary(binary) => write!(f, "{}", binary),
            Expr::Unary(unary) => write!(f, "{}", unary),
            Expr::Call(call) => write!(f, "{}", call),
            Expr::Ident(id) => write!(f, "{}", id),
            Expr::Float(n, _) => write!(f, "{}", n),
            Expr::Int(i, _) => write!(f, "{}", i),
            Expr::Str(s, _) => write!(f, "{}", s),
            Expr::Bool(b, _) => write!(f, "{}", b),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Var(var) => write!(f, "{}", var),
            Stmt::Assign(assign) => write!(f, "{}", assign),
            Stmt::Return(ret) => write!(f, "{}", ret),
            Stmt::Expr(expr) => write!(f, "{}", expr),
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

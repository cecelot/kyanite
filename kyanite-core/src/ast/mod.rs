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
    pub fn from_source(source: &Source) -> Result<Self, PipelineError> {
        let stream = TokenStream::from_source(source).map_err(|_| PipelineError::InvalidUtf8)?;
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
    Record(node::RecordDecl),
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
    Access(node::Access),
    Binary(node::Binary),
    Unary(node::Unary),
    Ident(node::Ident),
    Init(node::Init),
    Str(String, Token),
    Int(i64, Token),
    Float(f64, Token),
    Bool(bool, Token),
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
            Expr::Access(access) => access.chain.first().unwrap().start(),
            Expr::Call(call) => call.left.start(),
            Expr::Binary(binary) => binary.left.start(),
            Expr::Unary(unary) => unary.op.span.column,
            Expr::Ident(id) => id.name.span.column,
            Expr::Str(_, token) => token.span.column,
            Expr::Int(_, token) => token.span.column,
            Expr::Float(_, token) => token.span.column,
            Expr::Bool(_, token) => token.span.column,
            Expr::Init(init) => init.name.span.column,
        }
    }

    fn end(&self) -> usize {
        match self {
            Expr::Access(access) => access.chain.last().unwrap().end(),
            Expr::Call(call) => call.parens.1.span.column + 1,
            Expr::Binary(binary) => binary.right.end(),
            Expr::Unary(unary) => unary.right.end(),
            Expr::Ident(id) => id.name.span.column + id.name.span.length,
            Expr::Str(_, token) => token.span.column + token.span.length,
            Expr::Int(_, token) => token.span.column + token.span.length,
            Expr::Float(_, token) => token.span.column + token.span.length,
            Expr::Bool(_, token) => token.span.column + token.span.length,
            Expr::Init(init) => init.parens.1.span.column + 1,
        }
    }

    fn line(&self) -> usize {
        match self {
            Expr::Access(access) => access.chain.first().unwrap().line(),
            Expr::Call(call) => call.left.line(),
            Expr::Binary(binary) => binary.left.line(),
            Expr::Unary(unary) => unary.right.line(),
            Expr::Ident(id) => id.name.span.line,
            Expr::Str(_, token) => token.span.line,
            Expr::Int(_, token) => token.span.line,
            Expr::Float(_, token) => token.span.line,
            Expr::Bool(_, token) => token.span.line,
            Expr::Init(init) => init.name.span.line,
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
            Expr::Ident(_) => unimplemented!(),
            Expr::Init(..) => unimplemented!(),
            Expr::Access(..) => unimplemented!(),
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
    UserDefined(String),
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
            // TODO: this may be something other than a record in the future
            Type::UserDefined(name) => ir
                .records
                .get(name)
                .expect("called before all records built")
                .0
                .into(),
            Type::Void => unimplemented!("void does not implement `BasicTypeEnum`"),
        }
    }
}

impl From<&Type> for String {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Str => "str".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Void => "void".to_string(),
            Type::UserDefined(name) => name.clone(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from(self))
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
            name => Self::UserDefined(name.to_string()),
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

macro_rules! association {
    {$($ty:ident),*} => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
            pub struct $ty {
                pub name: Token,
                pub ty: Token,
            }

            impl $ty {
                pub fn new(name: Token, ty: Token) -> Self {
                    Self { name, ty }
                }
            }
        )*
    };
}

association! {
    Param,
    Field
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Initializer {
    pub name: Token,
    pub expr: Expr,
}

impl Initializer {
    pub fn new(name: Token, expr: Expr) -> Self {
        Self { name, expr }
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
                    let ast = ast::Ast::from_source(&Source::new($path)?)?;
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

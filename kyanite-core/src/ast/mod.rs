pub mod init;
pub mod node;
pub mod span;
#[cfg(test)]
mod strip;

use crate::{
    parse::Parser,
    token::{Lexer, Token},
    PipelineError, Source,
};
use std::{fmt, rc::Rc};

#[derive(Debug)]
pub struct Ast {
    pub nodes: Vec<Decl>,
}

impl Ast {
    fn new(lexer: Lexer) -> Result<Self, PipelineError> {
        let errors = lexer.errors.len();
        if errors > 0 {
            return Err(PipelineError::LexError(errors));
        }
        let mut parser = Parser::new(lexer.source, lexer.tokens);
        match parser.parse() {
            Ok(nodes) => Ok(Self { nodes }),
            Err(errors) => Err(PipelineError::ParseError(errors.len())),
        }
    }
}

impl TryFrom<&Source> for Ast {
    type Error = PipelineError;

    fn try_from(value: &Source) -> Result<Self, Self::Error> {
        let lexer = Lexer::from(value);
        Self::new(lexer)
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    Record(Rc<node::RecordDecl>),
    Function(Rc<node::FuncDecl>),
    Constant(Rc<node::ConstantDecl>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Var(Rc<node::VarDecl>),
    Assign(Rc<node::Assign>),
    Return(Rc<node::Return>),
    Expr(Expr),
    If(Rc<node::If>),
    While(Rc<node::While>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Rc<node::Call>),
    Access(Rc<node::Access>),
    Binary(Rc<node::Binary>),
    Unary(Rc<node::Unary>),
    Ident(Rc<node::Ident>),
    Init(Rc<node::Init>),
    Str(&'static str, Token),
    Int(i64, Token),
    Float(f64, Token),
    Bool(bool, Token),
}

impl Expr {
    pub fn ident(&self) -> &node::Ident {
        if let Expr::Ident(ident) = self {
            ident
        } else {
            panic!("called `Expr::ident()` on a non-ident")
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

impl From<&Token> for Type {
    fn from(value: &Token) -> Self {
        match value.lexeme.expect("token should have lexeme") {
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

impl PartialEq<Type> for Option<Token> {
    fn eq(&self, other: &Type) -> bool {
        match self {
            Some(token) => Type::from(token) == *other,
            None => *other == Type::Void,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Str => "str",
                Self::Int => "int",
                Self::Float => "float",
                Self::Bool => "bool",
                Self::Void => "void",
                Self::UserDefined(name) => name,
            }
        )
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
            Expr::Unary(unary) => unary.expr.ty(),
            Expr::Call(call) => call.left.ty(),
            Expr::Ident(_) => unimplemented!(),
            Expr::Init(..) => unimplemented!(),
            Expr::Access(..) => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub struct Initializer {
    pub name: Token,
    pub expr: Expr,
}

impl Initializer {
    pub fn new(name: Token, expr: Expr) -> Self {
        Self { name, expr }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: Token,
    pub ty: Token,
}

impl Field {
    pub fn new(name: Token, ty: Token) -> Self {
        Self { name, ty }
    }
}

macro_rules! assert_ast {
    ($($path:expr => $name:ident),*) => {
        #[cfg(test)]
        mod tests {
            use crate::{Source, ast::{self, strip::StripId}};

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let mut ast = ast::Ast::try_from(&Source::new($path)?)?;
                    ast.nodes.iter_mut().for_each(|node| node.strip_id());
                    insta::with_settings!({snapshot_path => "../../snapshots"}, {
                        insta::assert_debug_snapshot!(ast);
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

pub mod node;
pub mod span;
#[cfg(test)]
mod strip;
pub mod ty;

use crate::{parse::Parser, token::Lexer, PipelineError, Source};
use std::rc::Rc;

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
    Function(Rc<node::FuncDecl>),
    Class(Rc<node::ClassDecl>),
    Constant(Rc<node::ConstantDecl>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Var(Rc<node::VarDecl>),
    Assign(Rc<node::Assign>),
    Return(Rc<node::Return>),
    Expr(Expr),
    If(Rc<node::If>),
    While(Rc<node::While>),
    For(Rc<node::For>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Call(Rc<node::Call>),
    Access(Rc<node::Access>),
    Binary(Rc<node::Binary>),
    Unary(Rc<node::Unary>),
    Ident(Rc<node::Ident>),
    Init(Rc<node::Init>),
    Range(Rc<node::Range>),
    Str(Rc<node::Literal<&'static str>>),
    Int(Rc<node::Literal<i64>>),
    Float(Rc<node::Literal<f64>>),
    Bool(Rc<node::Literal<bool>>),
}

impl Expr {
    pub fn ident(&self) -> &node::Ident {
        if let Expr::Ident(ident) = self {
            ident
        } else {
            panic!("called `Expr::ident()` on a non-ident")
        }
    }

    pub fn init(&self) -> &node::Init {
        if let Expr::Init(init) = self {
            init
        } else {
            panic!("called `Expr::init()` on a non-init")
        }
    }

    pub fn range(&self) -> &node::Range {
        if let Expr::Range(range) = self {
            range
        } else {
            panic!("called `Expr::range()` on a non-range")
        }
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
    // FIXME: StripId doesn't work for this case.
    // "test-cases/access.kya" => access,
    "test-cases/mixed.kya" => mixed
);

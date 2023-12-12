use crate::{
    ast::{Expr, Stmt},
    token::Span,
};

pub trait Underline {
    fn span(&self) -> Span {
        Span::new(self.line(), self.start(), self.end() - self.start())
    }
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn line(&self) -> usize;
}

impl Underline for Stmt {
    fn start(&self) -> usize {
        match self {
            Stmt::Var(var) => var.name.span.column,
            Stmt::Assign(assign) => assign.target.start(),
            Stmt::Return(ret) => ret.keyword.span.column,
            Stmt::Expr(expr) => expr.start(),
            Stmt::If(cond) => cond.condition.start(),
            Stmt::While(cond) => cond.condition.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Stmt::Var(var) => var.expr.end(),
            Stmt::Assign(assign) => assign.expr.end(),
            Stmt::Return(ret) => ret.expr.end(),
            Stmt::Expr(expr) => expr.end(),
            Stmt::If(cond) => cond.condition.end(),
            Stmt::While(cond) => cond.condition.end(),
        }
    }

    fn line(&self) -> usize {
        match self {
            Stmt::Var(var) => var.name.span.line,
            Stmt::Assign(assign) => assign.target.line(),
            Stmt::Return(ret) => ret.expr.line(),
            Stmt::Expr(expr) => expr.line(),
            Stmt::If(cond) => cond.condition.line(),
            Stmt::While(cond) => cond.condition.line(),
        }
    }
}

impl Underline for Expr {
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
            Expr::Unary(unary) => unary.expr.end(),
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
            Expr::Unary(unary) => unary.expr.line(),
            Expr::Ident(id) => id.name.span.line,
            Expr::Str(_, token) => token.span.line,
            Expr::Int(_, token) => token.span.line,
            Expr::Float(_, token) => token.span.line,
            Expr::Bool(_, token) => token.span.line,
            Expr::Init(init) => init.name.span.line,
        }
    }
}

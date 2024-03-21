use crate::{
    ast::{ty::Type, Expr, Stmt},
    token::Span,
};

pub trait Combined {
    fn span(&self) -> Span {
        assert!(self.end() >= self.start(), "likely a multi-line span");
        Span::new(self.line(), self.start(), self.end() - self.start())
    }
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn line(&self) -> usize;
}

impl Combined for Type {
    fn start(&self) -> usize {
        self.base.span.column
    }

    fn end(&self) -> usize {
        self.params
            .last()
            .map_or(self.base.span.column + self.base.span.length, Combined::end)
    }

    fn line(&self) -> usize {
        self.base.span.line
    }
}

impl Combined for Stmt {
    fn start(&self) -> usize {
        match self {
            Stmt::Var(var) => var.name.span.column,
            Stmt::Assign(assign) => assign.target.start(),
            Stmt::Return(ret) => ret.keyword.span.column,
            Stmt::Expr(expr) => expr.start(),
            Stmt::If(cond) => cond.condition.start(),
            Stmt::While(l) => l.condition.start(),
            Stmt::For(l) => l.iter.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Stmt::Var(var) => var.expr.end(),
            Stmt::Assign(assign) => assign.expr.end(),
            Stmt::Return(ret) => ret.expr.end(),
            Stmt::Expr(expr) => expr.end(),
            Stmt::If(cond) => cond.condition.end(),
            Stmt::While(l) => l.condition.end(),
            Stmt::For(l) => l.iter.end(),
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
            Stmt::For(cond) => cond.iter.line(),
        }
    }
}

impl Combined for Expr {
    fn start(&self) -> usize {
        match self {
            Expr::Access(access) => access.chain.first().unwrap().start(),
            Expr::Call(call) => call.left.start(),
            Expr::Binary(binary) => binary.left.start(),
            Expr::Range(range) => range.start.start(),
            Expr::Unary(unary) => unary.op.span.column,
            Expr::Ident(id) => id.name.span.column,
            Expr::Str(s) => s.token.span.column,
            Expr::Int(i) => i.token.span.column,
            Expr::Float(f) => f.token.span.column,
            Expr::Bool(b) => b.token.span.column,
            Expr::Init(init) => init.name.span.column,
        }
    }

    fn end(&self) -> usize {
        match self {
            Expr::Range(range) => range.end.end(),
            Expr::Access(access) => access.chain.last().unwrap().end(),
            Expr::Call(call) => call.parens.1.span.column + 1,
            Expr::Binary(binary) => binary.right.end(),
            Expr::Unary(unary) => unary.expr.end(),
            Expr::Ident(id) => id.name.span.column + id.name.span.length,
            Expr::Str(s) => s.token.span.column + s.token.span.length,
            Expr::Int(i) => i.token.span.column + i.token.span.length,
            Expr::Float(f) => f.token.span.column + f.token.span.length,
            Expr::Bool(b) => b.token.span.column + b.token.span.length,
            // TODO: support multi-line spans
            Expr::Init(init) => init.name.span.column + init.name.span.length,
        }
    }

    fn line(&self) -> usize {
        match self {
            Expr::Range(range) => range.start.line(),
            Expr::Access(access) => access.chain.first().unwrap().line(),
            Expr::Call(call) => call.left.line(),
            Expr::Binary(binary) => binary.left.line(),
            Expr::Unary(unary) => unary.expr.line(),
            Expr::Ident(id) => id.name.span.line,
            Expr::Str(s) => s.token.span.line,
            Expr::Int(i) => i.token.span.line,
            Expr::Float(f) => f.token.span.line,
            Expr::Bool(b) => b.token.span.line,
            Expr::Init(init) => init.name.span.line,
        }
    }
}

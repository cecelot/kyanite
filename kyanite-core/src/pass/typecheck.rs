use std::ops::Deref;

use crate::{
    ast::{node, Decl, Expr, NodeSpan, Stmt, Type},
    reporting::error::PreciseError,
    token::{Span, Token, TokenKind},
    Source,
};

use super::symbol::{Binding, SymbolTable};

pub struct TypeCheckPass<'a> {
    source: &'a Source,
    program: &'a Vec<Decl>,
    errors: Vec<PreciseError>,
    function: Option<Token>,
    scopes: Vec<SymbolTable>,
}

trait Check {
    fn check(&self, pass: &mut TypeCheckPass<'_>) -> Type;
}

impl Check for Decl {
    fn check(&self, pass: &mut TypeCheckPass<'_>) -> Type {
        match self {
            Decl::Function(fun) => pass.function(fun),
            Decl::Constant(c) => pass.constant(c),
            Decl::Record(_) => todo!(),
        }
    }
}

impl Check for Stmt {
    fn check(&self, pass: &mut TypeCheckPass<'_>) -> Type {
        match self {
            Stmt::Return(r) => pass.ret(r),
            Stmt::Expr(e) => e.check(pass),
            Stmt::Var(v) => pass.var(v),
            Stmt::Assign(_) => Type::Void,
        }
    }
}

impl Check for Expr {
    fn check(&self, pass: &mut TypeCheckPass<'_>) -> Type {
        match self {
            Expr::Binary(b) => pass.binary(b),
            Expr::Unary(u) => pass.unary(u),
            Expr::Call(c) => pass.call(c),
            Expr::Ident(i) => pass.ident(i),
            Expr::Init(..) => todo!(),
            Expr::Bool(..) => Type::Bool,
            Expr::Int(..) => Type::Int,
            Expr::Float(..) => Type::Float,
            Expr::Str(..) => Type::Str,
        }
    }
}

impl<'a> TypeCheckPass<'a> {
    pub fn new(table: SymbolTable, source: &'a Source, program: &'a Vec<Decl>) -> Self {
        Self {
            source,
            program,
            errors: vec![],
            function: None,
            scopes: vec![table],
        }
    }

    pub fn run(&mut self) -> Result<(), usize> {
        for node in self.program {
            node.check(self);
        }
        let len = self.errors.len();
        if len > 0 {
            Err(len)
        } else {
            Ok(())
        }
    }

    fn scope_mut(&mut self) -> &mut SymbolTable {
        self.scopes.last_mut().unwrap()
    }

    fn begin_scope(&mut self) {
        self.scopes.push(SymbolTable::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn symbol(&self, name: &Token) -> Option<&Binding> {
        for scope in self.scopes.iter().rev() {
            if let Some(definition) = scope.get(name) {
                return Some(definition);
            }
        }
        None
    }

    fn error(&mut self, at: Span, heading: String, text: String) {
        let error = PreciseError::new(self.source, at, heading, text);
        println!("{}", error);
        self.errors.push(error);
    }

    fn unary(&mut self, unary: &node::Unary) -> Type {
        let got = unary.right.check(self);
        match unary.op.kind {
            TokenKind::Minus => {
                if !matches!(got, Type::Int | Type::Float) {
                    self.error(
                        unary.right.span(),
                        format!("cannot negate {}", got),
                        format!("expression of type {}", got),
                    );
                }
                got
            }
            TokenKind::Bang => {
                if got != Type::Bool {
                    self.error(
                        unary.right.span(),
                        format!("cannot invert {}", got),
                        format!("expression of type {}", got),
                    );
                }
                Type::Bool
            }
            _ => unimplemented!(),
        }
    }

    fn constant(&mut self, c: &node::ConstantDecl) -> Type {
        let got = c.expr.check(self);
        let expected = Type::from(&c.ty);
        if got != expected {
            self.error(
                c.expr.span(),
                format!("expected initializer to be of type {}", expected),
                format!("expression of type {}", got),
            );
        }
        expected
    }

    fn var(&mut self, v: &node::VarDecl) -> Type {
        let got = v.expr.check(self);
        let expected = Type::from(&v.ty);
        if got != expected {
            self.error(
                v.expr.span(),
                format!("expected initializer to be of type {}", expected),
                format!("expression of type {}", got),
            );
        }
        self.scope_mut()
            .insert(v.name.clone(), Binding::Variable(v.clone()));
        expected
    }

    fn function(&mut self, fun: &node::FuncDecl) -> Type {
        if String::from(&fun.name) == "main" {
            if let Some(ty) = &fun.ty {
                if String::from(ty) != "void" {
                    self.error(
                        ty.span,
                        "main function must return void".into(),
                        "try changing or removing this type".into(),
                    );
                }
            }
        }
        self.begin_scope();
        self.function = Some(fun.name.clone());
        for param in &fun.params {
            self.scope_mut()
                .insert(param.name.clone(), Binding::Function(fun.clone()));
        }
        for node in &fun.body {
            node.check(self);
        }
        self.end_scope();
        self.function = None;
        Type::Void
    }

    fn ret(&mut self, r: &node::Return) -> Type {
        let got = r.expr.check(self);
        match &self.function {
            Some(function) => {
                let symbol = self.symbol(function).unwrap();
                if got != symbol.ty() {
                    self.error(
                        r.expr.span(),
                        format!("expected return type to be {}", symbol.ty()),
                        format!("expression is of type {}", got),
                    );
                }
            }
            None => unimplemented!("disallowed by parser"),
        }
        got
    }

    fn binary(&mut self, b: &node::Binary) -> Type {
        let lty = b.left.check(self);
        let rty = b.right.check(self);
        if lty != rty {
            let heading = match b.op.kind {
                TokenKind::Plus => format!("cannot add {} to {}", lty, rty),
                TokenKind::Minus => format!("cannot subtract {} from {}", rty, lty),
                TokenKind::Star => format!("cannot multiply {} by {}", lty, rty),
                TokenKind::Slash => format!("cannot divide {} by {}", lty, rty),
                _ => format!("cannot compare {} and {}", lty, rty),
            };
            self.error(b.op.span, heading, "".into());
        }
        if matches!(
            b.op.kind,
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash
        ) {
            lty
        } else {
            Type::Bool
        }
    }

    fn ident(&mut self, id: &node::Ident) -> Type {
        match self.symbol(&id.name) {
            Some(Binding::Function(f)) => {
                let param = f.params.iter().find(|p| p.name == id.name).unwrap();
                Type::from(&param.ty)
            }
            Some(Binding::Variable(v)) => Type::from(&v.ty),
            Some(Binding::Constant(c)) => Type::from(&c.ty),
            _ => {
                self.error(id.name.span, "is not defined".into(), "".into());
                Type::Void
            }
        }
    }

    fn call(&mut self, call: &node::Call) -> Type {
        let name = match &call.left.deref() {
            Expr::Ident(ident) => ident.name.clone(),
            Expr::Binary(_) => todo!("member access"),
            _ => unimplemented!(),
        };
        let (arity, params, ty) = match self.symbol(&name) {
            Some(Binding::Function(function)) => (
                function.params.len(),
                function.params.clone(),
                function.ty.clone(),
            ),
            Some(_) => {
                self.error(
                    call.parens.0.span,
                    format!("{} is not a function", name),
                    "".into(),
                );
                return Type::Void;
            }
            _ => {
                let name = String::from(&name);
                // TODO: more robust type checking for builtins
                if name != "println" && name != "max" && name != "min" {
                    self.error(
                        call.parens.0.span,
                        format!("{} is not defined", name),
                        "".into(),
                    );
                }
                return Type::Void;
            }
        };
        if arity != call.args.len() {
            self.error(
                call.left.span(),
                format!(
                    "this function takes {} arguments, but {} were provided",
                    arity,
                    call.args.len()
                ),
                format!("while calling {} here", name),
            );
        }
        for (i, arg) in call.args.iter().enumerate() {
            let got = arg.check(self);
            if i < params.len() {
                let expected = Type::from(&params[i].ty);
                if got != expected {
                    self.error(
                        arg.span(),
                        format!("expected argument of type {}, but found {}", expected, got),
                        format!("expression of type {}", got),
                    );
                }
            }
        }
        if let Some(ty) = ty {
            Type::from(&ty)
        } else {
            Type::Void
        }
    }
}

macro_rules! assert_typecheck {
    ($($path:expr => $name:ident),*) => {
        #[cfg(test)]
        mod tests {
            use crate::{SymbolTable, pass::typecheck::TypeCheckPass};

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let source = crate::Source::new($path)?;
                    let ast = crate::ast::Ast::from_source(source.clone())?;
                    let symbols = SymbolTable::from(&ast.nodes);
                    let mut pass = TypeCheckPass::new(symbols, &source, &ast.nodes);
                    let _ = pass.run();
                    insta::with_settings!({snapshot_path => "../snapshots"}, {
                        insta::assert_yaml_snapshot!(pass.errors);
                    });

                    Ok(())
                }
            )*
        }
    };
}

assert_typecheck! {
    "test-cases/typecheck/varied.kya" => varied
}

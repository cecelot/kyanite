use std::ops::Deref;

use crate::{
    ast::{node, File, Node, NodeSpan, Type},
    reporting::error::PreciseError,
    token::{Span, Token, TokenKind},
    Source,
};

use super::symbol::SymbolTable;

pub struct TypeCheckPass<'a> {
    source: &'a Source,
    program: &'a File,
    errors: Vec<PreciseError>,
    function: Option<Token>,
    scopes: Vec<SymbolTable>,
}

impl<'a> TypeCheckPass<'a> {
    pub fn new(table: SymbolTable, source: &'a Source, program: &'a File) -> Self {
        Self {
            source,
            program,
            errors: vec![],
            function: None,
            scopes: vec![table],
        }
    }

    pub fn run(&mut self) -> Result<(), usize> {
        for node in &self.program.nodes {
            self.check(node);
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

    fn symbol(&self, name: &Token) -> Option<&Node> {
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

    fn check(&mut self, node: &Node) -> Type {
        match node {
            Node::Float(..) => Type::Float,
            Node::Int(..) => Type::Int,
            Node::Bool(..) => Type::Bool,
            Node::Str(..) => Type::Str,
            Node::ConstantDecl(c) => self.constant(c),
            Node::VarDecl(v) => self.var(v),
            Node::FuncDecl(fun) => self.function(fun),
            Node::Return(r) => self.ret(r),
            Node::Binary(b) => self.binary(b),
            Node::Ident(id) => self.ident(id),
            Node::Call(call) => self.call(call),
            e => todo!("{:?}", e),
        }
    }

    fn constant(&mut self, c: &node::ConstantDecl) -> Type {
        let got = self.check(&c.expr);
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
        let got = self.check(&v.expr);
        let expected = Type::from(&v.ty);
        if got != expected {
            self.error(
                v.expr.span(),
                format!("expected initializer to be of type {}", expected),
                format!("expression of type {}", got),
            );
        }
        self.scope_mut()
            .insert(v.name.clone(), Node::VarDecl(v.clone()));
        expected
    }

    fn function(&mut self, fun: &node::FuncDecl) -> Type {
        self.begin_scope();
        self.function = Some(fun.name.clone());
        for param in &fun.params {
            self.scope_mut()
                .insert(param.name.clone(), Node::FuncDecl(fun.clone()));
        }
        for node in &fun.body {
            self.check(node);
        }
        self.end_scope();
        self.function = None;
        Type::Void
    }

    fn ret(&mut self, r: &node::Return) -> Type {
        let got = self.check(&r.expr);
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
        let lty = self.check(&b.left);
        let rty = self.check(&b.right);
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
        lty
    }

    fn ident(&mut self, id: &node::Ident) -> Type {
        match self.symbol(&id.name) {
            Some(Node::FuncDecl(f)) => {
                let param = f.params.iter().find(|p| p.name == id.name).unwrap();
                Type::from(&param.ty)
            }
            Some(Node::VarDecl(v)) => Type::from(&v.ty),
            Some(Node::ConstantDecl(c)) => Type::from(&c.ty),
            Some(node) => unimplemented!("{:?} is not implemented as an identifier", node),
            _ => {
                self.error(id.name.span, "is not defined".into(), "".into());
                Type::Void
            }
        }
    }

    fn call(&mut self, call: &node::Call) -> Type {
        let name = match &call.left.deref() {
            Node::Ident(ident) => ident.name.clone(),
            Node::Binary(_) => todo!(),
            _ => unimplemented!(),
        };
        let (arity, params, ty) = match self.symbol(&name) {
            Some(Node::FuncDecl(function)) => (
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
            let got = self.check(arg);
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
                    let symbols = SymbolTable::from(&ast.file);
                    let mut pass = TypeCheckPass::new(symbols, &source, &ast.file);
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

use std::ops::Deref;

use crate::{
    ast::{node, File, Node, Type},
    token::Token,
};

use super::symbol::{Symbol, SymbolTable};

pub struct TypeCheckPass<'a> {
    program: &'a File,
    errors: usize,
    function: Option<String>,
    scopes: Vec<SymbolTable>,
}

impl<'a> TypeCheckPass<'a> {
    pub fn new(table: SymbolTable, program: &'a File) -> Self {
        Self {
            program,
            errors: 0,
            function: None,
            scopes: vec![table],
        }
    }

    pub fn run(&mut self) -> usize {
        for node in &self.program.nodes {
            self.check(node);
        }
        self.errors
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

    fn symbol(&self, name: &String) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn error(&mut self, at: &Token, msg: String) {
        eprintln!("{} at {}", msg, at.span);
        self.errors += 1;
    }

    fn check(&mut self, node: &Node) -> Type {
        match node {
            Node::Float(_) => Type::Float,
            Node::Int(_) => Type::Int,
            Node::Bool(_) => Type::Bool,
            Node::Str(_) => Type::Str,
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
                &c.ty,
                format!(
                    "Mismatched types in constant declaration (expected {:?}, got {:?})",
                    Type::from(&c.ty),
                    got,
                ),
            );
        }
        expected
    }

    fn var(&mut self, v: &node::VarDecl) -> Type {
        let got = self.check(&v.expr);
        let expected = Type::from(&v.ty);
        if got != expected {
            self.error(
                &v.ty,
                format!(
                    "Mismatched types in variable declaration (expected {:?}, got {:?})",
                    Type::from(&v.ty),
                    got,
                ),
            );
        }
        self.scope_mut()
            .insert(String::from(&v.name), Symbol::Variable(Type::from(&v.ty)));
        expected
    }

    fn function(&mut self, fun: &node::FuncDecl) -> Type {
        self.begin_scope();
        self.function = Some(String::from(&fun.name));
        for param in &fun.params {
            self.scope_mut().insert(
                String::from(&param.name),
                Symbol::Variable(Type::from(&param.ty)),
            );
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
                        &r.keyword,
                        format!(
                            "Mismatched types in return (expected {:?}, got {:?})",
                            symbol.ty(),
                            got
                        ),
                    );
                }
            }
            None => self.error(&r.keyword, "Return outside of function".into()),
        }
        got
    }

    fn binary(&mut self, b: &node::Binary) -> Type {
        let lty = self.check(&b.left);
        let rty = self.check(&b.right);
        if lty != rty {
            self.error(
                &b.op,
                format!(
                    "Mismatched types in '{}' operation ({:?}, {:?})",
                    b.op, lty, rty
                ),
            );
        }
        lty
    }

    fn ident(&mut self, id: &node::Ident) -> Type {
        let name = String::from(&id.name);
        match self.symbol(&name) {
            Some(Symbol::Variable(ty)) => ty.clone(),
            _ => {
                self.error(&id.name, format!("{} is not defined", name));
                Type::Void
            }
        }
    }

    fn call(&mut self, call: &node::Call) -> Type {
        let name = match &call.left.deref() {
            Node::Ident(ident) => String::from(&ident.name),
            Node::Binary(_) => todo!(),
            _ => unimplemented!(),
        };
        let (arity, params, ty) = match self.symbol(&name) {
            Some(Symbol::Function(function)) => {
                (function.arity, function.params.clone(), function.ty.clone())
            }
            Some(_) => {
                self.error(&call.parens.0, format!("{} is not a function", name));
                return Type::Void;
            }
            _ => {
                // TODO: more robust type checking for builtins
                if name != "println" && name != "max" && name != "min" {
                    self.error(&call.parens.0, format!("{} is not defined", name));
                }
                return Type::Void;
            }
        };
        if arity != call.args.len() {
            self.error(
                &call.parens.1,
                format!("Expected {} arguments, got {}", arity, call.args.len()),
            );
        }
        for (i, arg) in call.args.iter().enumerate() {
            let ty = self.check(arg);
            if i < params.len() && ty != params[i].1 {
                let token = match i {
                    0 => &call.parens.0,
                    _ if i == params.len() - 1 => &call.parens.1,
                    _ => &call.delimiters[i],
                };
                self.error(
                    token,
                    format!(
                        "Mismatched type in function parameter: (expected {:?}, got {:?})",
                        params[i].1, ty
                    ),
                );
            }
        }
        ty.clone()
    }
}

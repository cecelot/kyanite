use std::collections::HashMap;

use crate::ast::{node, File, Node, Type};

#[derive(Debug)]
pub struct Function {
    pub(super) arity: usize,
    pub(super) ty: Type,
    pub(super) params: Vec<(String, Type)>,
}

impl Function {
    pub fn new(arity: usize, ty: Type, params: Vec<(String, Type)>) -> Self {
        Self { arity, ty, params }
    }
}

#[derive(Debug)]
pub enum Symbol {
    Function(Function),
    Variable(Type),
}

impl Symbol {
    pub fn ty(&self) -> Type {
        match self {
            Symbol::Function(f) => f.ty.clone(),
            Symbol::Variable(ty) => ty.clone(),
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable(HashMap<String, Symbol>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&self, name: &String) -> Option<&Symbol> {
        self.0.get(name)
    }

    pub fn insert(&mut self, name: String, symbol: Symbol) {
        self.0.insert(name, symbol);
    }
}

impl From<Vec<(String, Symbol)>> for SymbolTable {
    fn from(vars: Vec<(String, Symbol)>) -> Self {
        Self(vars.into_iter().collect())
    }
}

impl From<&File> for SymbolTable {
    fn from(program: &File) -> Self {
        let mut table = Self::new();
        for node in &program.nodes {
            self::SymbolTableVisitor::visit(node, &mut table);
        }
        // dbg!(&table);
        table
    }
}

trait SymbolTableVisitor {
    fn visit(&self, table: &mut SymbolTable);
}

impl SymbolTableVisitor for Node {
    fn visit(&self, table: &mut SymbolTable) {
        match self {
            Node::FuncDecl(fun) => func(fun, table),
            Node::ConstantDecl(c) => constant(c, table),
            _ => {}
        }
    }
}

fn func(fun: &node::FuncDecl, table: &mut SymbolTable) {
    let params: Vec<(String, Type)> = fun
        .params
        .iter()
        .map(|param| (String::from(&param.name), Type::from(&param.ty)))
        .collect();
    table.insert(
        String::from(&fun.name),
        Symbol::Function(Function::new(
            fun.params.len(),
            Type::from(fun.ty.as_ref()),
            params,
        )),
    );
}

fn constant(c: &node::ConstantDecl, table: &mut SymbolTable) {
    table.insert(String::from(&c.name), Symbol::Variable(Type::from(&c.ty)));
}

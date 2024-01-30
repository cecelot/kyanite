use crate::ast::{node, Decl, Type};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub enum Symbol {
    Record(Rc<node::RecordDecl>),
    Function(Rc<node::FuncDecl>),
    Constant(Rc<node::ConstantDecl>),
    Variable(Rc<node::VarDecl>),
}

impl Symbol {
    pub fn record(&self) -> &node::RecordDecl {
        match self {
            Symbol::Record(rec) => rec,
            _ => panic!("called `Symbol::record()` on a non-record symbol"),
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Self::Record(rec) => Type::from(&rec.name),
            Self::Function(fun) => fun.ty.as_ref().into(),
            Self::Constant(c) => Type::from(&c.ty),
            Self::Variable(v) => Type::from(&v.ty),
        }
    }
}

crate::newtype!(SymbolTable:HashMap<String, Symbol>);

impl From<&Vec<Decl>> for SymbolTable {
    fn from(nodes: &Vec<Decl>) -> Self {
        let mut table = Self(HashMap::new());
        for node in nodes {
            self::SymbolTableVisitor::visit(node, &mut table);
        }
        table
    }
}

trait SymbolTableVisitor {
    fn visit(&self, table: &mut SymbolTable);
}

impl SymbolTableVisitor for Decl {
    fn visit(&self, table: &mut SymbolTable) {
        match self {
            Decl::Function(fun) => func(fun, table),
            Decl::Constant(c) => constant(c, table),
            Decl::Record(rec) => record(rec, table),
        }
    }
}

fn func(fun: &Rc<node::FuncDecl>, table: &mut SymbolTable) {
    table.insert(fun.name.to_string(), Symbol::Function(Rc::clone(fun)));
}

fn constant(c: &Rc<node::ConstantDecl>, table: &mut SymbolTable) {
    table.insert(c.name.to_string(), Symbol::Constant(Rc::clone(c)));
}

fn record(rec: &Rc<node::RecordDecl>, table: &mut SymbolTable) {
    table.insert(rec.name.to_string(), Symbol::Record(Rc::clone(rec)));
}

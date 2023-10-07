use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::{
    ast::{node, Decl, Type},
    token::Token,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Binding {
    Function(node::FuncDecl),
    Constant(node::ConstantDecl),
    Variable(node::VarDecl),
}

impl Binding {
    pub fn ty(&self) -> Type {
        match self {
            Self::Function(fun) => Type::from(fun.ty.as_ref()),
            Self::Constant(c) => Type::from(&c.ty),
            Self::Variable(v) => Type::from(&v.ty),
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable(HashMap<Token, Binding>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

impl Deref for SymbolTable {
    type Target = HashMap<Token, Binding>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SymbolTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl From<&Vec<Decl>> for SymbolTable {
    fn from(nodes: &Vec<Decl>) -> Self {
        let mut table = Self::new();
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
        }
    }
}

fn func(fun: &node::FuncDecl, table: &mut SymbolTable) {
    table.insert(fun.name.clone(), Binding::Function(fun.clone()));
}

fn constant(c: &node::ConstantDecl, table: &mut SymbolTable) {
    table.insert(c.name.clone(), Binding::Constant(c.clone()));
}

use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::{
    ast::{node, File, Node},
    token::Token,
};

#[derive(Debug)]
pub struct SymbolTable(HashMap<Token, Node>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

impl Deref for SymbolTable {
    type Target = HashMap<Token, Node>;

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

impl From<Vec<(Token, Node)>> for SymbolTable {
    fn from(vars: Vec<(Token, Node)>) -> Self {
        Self(vars.into_iter().collect())
    }
}

impl From<&File> for SymbolTable {
    fn from(program: &File) -> Self {
        let mut table = Self::new();
        for node in &program.nodes {
            self::SymbolTableVisitor::visit(node, &mut table);
        }
        table
    }
}

trait SymbolTableVisitor {
    fn visit(&self, table: &mut SymbolTable);
}

impl SymbolTableVisitor for Node {
    fn visit(&self, table: &mut SymbolTable) {
        match self {
            decl @ Node::FuncDecl(fun) => func(fun, decl.clone(), table),
            decl @ Node::ConstantDecl(c) => constant(c, decl.clone(), table),
            _ => {}
        }
    }
}

fn func(fun: &node::FuncDecl, decl: Node, table: &mut SymbolTable) {
    table.insert(fun.name.clone(), decl);
}

fn constant(c: &node::ConstantDecl, decl: Node, table: &mut SymbolTable) {
    table.insert(c.name.clone(), decl);
}

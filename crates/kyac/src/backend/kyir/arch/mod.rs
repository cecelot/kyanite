pub mod armv8a;

use std::collections::HashMap;

use crate::{
    ast::node::FuncDecl,
    backend::kyir::{Expr, Instr},
};

pub trait Frame {
    fn new(function: &FuncDecl) -> Self
    where
        Self: Sized;
    fn allocate(&mut self, ident: &str, ptr: bool) -> Expr;
    fn get(&self, ident: &str) -> Expr;
    fn map(&self) -> HashMap<Location, bool>;
    fn prologue(&self) -> Vec<Instr>;
    fn epilogue(&self) -> Vec<Instr>;
    fn prefixed(call: &str) -> String;
    fn registers() -> RegisterMap;
    fn header() -> &'static str;
    fn label(&self) -> &String;
    fn offset(&self) -> i64;
    fn word_size() -> usize;
}

pub struct RegisterMap {
    pub callee: &'static [&'static str],
    pub temporary: &'static [&'static str],
    pub argument: &'static [&'static str],
    pub ret: ReturnRegisters,
    pub stack: &'static str,
}

pub struct ReturnRegisters {
    pub address: &'static str,
    pub value: &'static str,
}

impl RegisterMap {
    pub fn all(&self) -> Vec<&str> {
        self.callee
            .iter()
            .chain(self.temporary.iter())
            .chain(self.argument.iter())
            .chain([self.ret.address, self.ret.value, self.stack].iter())
            .copied()
            .collect()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Location {
    Frame(i64),
}

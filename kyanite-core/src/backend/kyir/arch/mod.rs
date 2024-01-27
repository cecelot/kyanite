use crate::{
    ast::{node::FuncDecl, Type},
    backend::kyir::Instr,
    pass::SymbolTable,
};

use super::Expr;

pub mod amd64;

pub struct ReturnRegisters {
    pub address: &'static str,
    pub value: &'static str,
}

pub struct RegisterMap {
    pub callee: &'static [&'static str],
    pub caller: &'static [&'static str],
    pub temporary: &'static [&'static str],
    pub argument: &'static [&'static str],
    pub ret: ReturnRegisters,
    pub frame: &'static str,
    pub stack: &'static str,
}

impl RegisterMap {
    pub fn all(&self) -> Vec<&str> {
        let mut all = self.callee.to_vec();
        all.extend_from_slice(self.caller);
        all.extend_from_slice(self.temporary);
        all.extend_from_slice(self.argument);
        all.push(self.ret.address);
        all.push(self.ret.value);
        all.push(self.frame);
        all.push(self.stack);
        all
    }
}

pub trait Frame {
    fn new(function: &FuncDecl) -> Self
    where
        Self: Sized;
    fn allocate(&mut self, symbols: &SymbolTable, ident: &str, ty: Option<&Type>) -> Expr;
    fn get(&self, ident: &str, temp: Option<String>, index: Option<usize>) -> Expr;
    fn get_offset(&self, ident: &str) -> i64;
    fn offset(&self) -> i64;
    fn prologue(&self) -> Vec<Instr>;
    fn epilogue(&self) -> Vec<Instr>;
    fn header() -> String;
    fn registers() -> RegisterMap;
    fn word_size() -> usize;
}

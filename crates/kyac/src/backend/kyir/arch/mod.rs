pub mod amd64;

use crate::{
    ast::{node::FuncDecl, Type},
    backend::kyir::{Codegen, Expr, Instr},
    pass::SymbolTable,
};

pub trait Frame {
    fn new(function: &FuncDecl) -> Self
    where
        Self: Sized;
    fn allocate(&mut self, symbols: &SymbolTable, ident: &str, ty: Option<&Type>) -> Expr;
    fn get(&self, ident: &str, temp: Option<String>, index: Option<usize>) -> Expr;
    fn get_offset(&self, ident: &str) -> i64;
    fn offset(&self) -> i64;
    fn prologue(&self) -> Vec<Instr>;
    fn label(&self) -> &String;
    fn passes(codegen: &mut Codegen<Self>)
    where
        Self: Sized;
    fn epilogue(&self) -> Vec<Instr>;
    fn header() -> String;
    fn registers() -> RegisterMap;
    fn word_size() -> usize;
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

pub struct ReturnRegisters {
    pub address: &'static str,
    pub value: &'static str,
}

impl RegisterMap {
    pub fn all(&self) -> Vec<&str> {
        self.callee
            .iter()
            .chain(self.caller.iter())
            .chain(self.temporary.iter())
            .chain(self.argument.iter())
            .chain([self.ret.address, self.ret.value, self.frame, self.stack].iter())
            .copied()
            .collect()
    }
}

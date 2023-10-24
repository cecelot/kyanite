use crate::{
    ast::{node::FuncDecl, Type},
    pass::SymbolTable,
};

use super::Expr;

pub(super) mod amd64;

pub struct ReturnRegisters {
    pub address: &'static str,
    pub value: &'static str,
}

pub struct RegisterMap {
    pub callee: Vec<&'static str>,
    pub caller: Vec<&'static str>,
    pub temporary: Vec<&'static str>,
    pub argument: Vec<&'static str>,
    pub ret: ReturnRegisters,
    pub frame: &'static str,
    pub stack: &'static str,
}

pub trait Frame {
    fn new(function: &FuncDecl) -> Self
    where
        Self: Sized;
    fn allocate(&mut self, symbols: &SymbolTable, ident: &str, ty: Option<&Type>) -> Box<Expr>;
    fn get(&self, ident: &str, temp: Option<String>, index: Option<usize>) -> Box<Expr>;
    fn get_offset(&self, ident: &str) -> i64;
    fn offset(&self) -> i64;
    fn prologue(&self) -> String;
    fn epilogue(&self) -> String;
    fn registers() -> RegisterMap;
    fn word_size() -> usize;
}

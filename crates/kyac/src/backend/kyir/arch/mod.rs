pub mod armv8a;

use crate::{
    ast::node::FuncDecl,
    backend::kyir::{alloc::Registers, AsmInstr, Expr, RelOp},
};
use std::{collections::HashMap, fmt};

pub trait Frame<I: ArchInstr> {
    fn new(function: &FuncDecl) -> Self
    where
        Self: Sized;
    fn allocate(&mut self, ident: &str, ptr: bool) -> Expr;
    fn get(&self, ident: &str) -> Expr;
    fn map(&self) -> HashMap<Location, bool>;
    fn prologue(&self) -> Vec<I>;
    fn epilogue(&self) -> Vec<I>;
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

pub trait ArchInstr: FlowGraphMeta + Format + fmt::Debug + fmt::Display {
    fn create_label(address: String) -> Self;
    fn data_fragment(kind: String, value: String) -> Self;
    fn load_fragment(dst: String, label: String) -> Self;
    fn load_from_frame(dst: String, offset: i64) -> Self;
    fn store_to_frame(src: String, offset: i64) -> Self;
    fn store_to_address(src: String, addr: String) -> Self;
    fn load_from_address(dst: String, addr: String) -> Self;
    /// `move` is a reserved keyword in Rust so we use an analogous term instead
    fn copy(dst: String, src: String) -> Self;
    fn copy_int(dst: String, value: i64) -> Self;
    fn add(dst: String, src: String) -> Self;
    fn sub(dst: String, src: String) -> Self;
    fn mul(dst: String, src: String) -> Self;
    fn div(dst: String, src: String) -> Self;
    fn compare(lhs: String, rhs: String) -> Self;
    fn create_jump(label: String) -> Self;
    fn conditional_jump(label: String, rel: RelOp) -> Self;
    fn call(ext: String) -> Self;
}

pub trait FlowGraphMeta {
    fn defines(&self) -> Vec<String>;
    fn uses(&self) -> Vec<String>;
    fn to(&self) -> Option<String>;
    fn jump(&self) -> bool;
    fn label(&self) -> Option<String>;
}

pub trait Format {
    fn format<I: ArchInstr, F: Frame<I>>(self, registers: &Registers) -> Self;
}

impl<I: ArchInstr> FlowGraphMeta for AsmInstr<I> {
    fn defines(&self) -> Vec<String> {
        self.inner.defines()
    }

    fn uses(&self) -> Vec<String> {
        self.inner.uses()
    }

    fn jump(&self) -> bool {
        self.inner.jump()
    }

    fn to(&self) -> Option<String> {
        self.inner.to()
    }

    fn label(&self) -> Option<String> {
        self.inner.label()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Location {
    Frame(i64),
}

mod ast;
mod backend;
pub mod compile; // FIXME: leaky
mod macros;
mod parse;
mod pass;
mod reporting;
pub mod subprocess; // FIXME: leaky
mod token;

use crate::{
    backend::{kyir, kyir::arch::amd64::Amd64, llvm},
    compile::{Asm, Ir},
    pass::{SymbolTable, TypeCheckPass},
};
use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(thiserror::Error, Debug)]
pub enum PipelineError {
    #[error("file \"{0}\" does not exist")]
    FileNotFound(&'static str),
    #[error("file is not valid UTF-8")]
    InvalidUtf8,
    #[error("(while lexing source) {0} error(s) encountered")]
    LexError(usize),
    #[error("(while parsing) {0} error(s) encountered")]
    ParseError(usize),
    #[error("(while type checking) {0} error(s) encountered")]
    TypeError(usize),
    #[error("(while building ir) {0}")]
    IrError(llvm::IrError),
    #[error("failed to compile (see output)")]
    CompileError(String),
}

pub struct Program<'a> {
    source: Source,
    writer: Option<&'a mut dyn Write>,
    llvm: bool,
}

impl<'a> Program<'a> {
    #[must_use]
    pub fn new(source: Source) -> Self {
        Self {
            source,
            writer: None,
            llvm: false,
        }
    }

    #[must_use]
    pub fn llvm(mut self, llvm: bool) -> Self {
        self.llvm = llvm;
        self
    }

    #[must_use]
    pub fn write_to(mut self, writer: &'a mut dyn Write) -> Self {
        self.writer = Some(writer);
        self
    }

    pub fn build(self) -> Result<String, PipelineError> {
        let mut stdout = std::io::stdout();
        let writer = self.writer.unwrap_or(&mut stdout);
        let mut ast = ast::Ast::try_from(&self.source)?;
        let filename: String = {
            let name: Vec<_> = self
                .source
                .filename
                .chars()
                .rev()
                .take_while(|&c| c != '/')
                .collect();
            name.iter().rev().collect()
        };
        let symbols = SymbolTable::from(&ast.nodes);
        let mut accesses = HashMap::new();
        let mut pass = TypeCheckPass::new(&symbols, &mut accesses, self.source, &ast.nodes);
        pass.run().map_err(PipelineError::TypeError)?;
        if self.llvm {
            let ir = Ir(llvm::Ir::new(&mut ast.nodes, symbols, accesses)
                .map_err(PipelineError::IrError)?
                .to_string());
            ir.compile(&filename, writer)
        } else {
            let assembly = Asm(kyir::asm::<Amd64>(&ast.nodes, &symbols, &accesses));
            assembly.compile::<Amd64>(&filename, writer)
        }
    }
}

impl TryFrom<PathBuf> for Program<'_> {
    type Error = PipelineError;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        let source = Source::new(path)?;
        Ok(Self::new(source))
    }
}

impl TryFrom<&str> for Program<'_> {
    type Error = PipelineError;

    fn try_from(path: &str) -> Result<Self, Self::Error> {
        let source = Source::new(path)?;
        Ok(Self::new(source))
    }
}

#[derive(Debug, Default, Clone)]
pub struct Source {
    filename: &'static str,
    chars: Vec<char>,
    raw: String,
}

impl Source {
    pub fn new<P>(path: P) -> Result<Self, PipelineError>
    where
        P: AsRef<Path>,
    {
        let filename = path
            .as_ref()
            .to_str()
            .ok_or(PipelineError::InvalidUtf8)?
            .to_string()
            .leak();
        let mut raw = String::new();
        let mut file = File::open(&path).map_err(|_| PipelineError::FileNotFound(filename))?;
        file.read_to_string(&mut raw)
            .map_err(|_| PipelineError::InvalidUtf8)?;
        Ok(Self {
            filename,
            chars: raw.chars().collect(),
            raw,
        })
    }

    #[must_use]
    pub fn in_memory(raw: String) -> Self {
        Self {
            filename: "in-memory.kya",
            chars: raw.chars().collect(),
            raw,
        }
    }
}

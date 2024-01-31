mod ast;
mod backend;
mod error;
mod macros;
mod parse;
mod pass;
mod token;

pub use crate::backend::kyir::arch::{amd64::Amd64, Frame};

use crate::{
    backend::{kyir, llvm},
    pass::{SymbolTable, TypeCheckPass},
};
use std::{collections::HashMap, fs::File, io::Read, path::Path};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn compile(source: &Source, backend: &Backend) -> Result<Output, PipelineError> {
    let mut ast = ast::Ast::try_from(source)?;
    let symbols = SymbolTable::from(&ast.nodes);
    let mut accesses = HashMap::new();
    let mut pass = TypeCheckPass::new(&symbols, &mut accesses, source, &ast.nodes);
    pass.run().map_err(PipelineError::TypeError)?;
    match backend {
        Backend::Llvm => Ok(Output::Llvm(
            llvm::Ir::build(&mut ast.nodes, symbols, accesses)
                .map_err(PipelineError::IrError)?
                .to_string(),
        )),
        Backend::Kyir => Ok(Output::Asm(kyir::asm::<Amd64>(
            &ast.nodes, &symbols, &accesses,
        ))),
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

    #[must_use]
    pub fn filename(&self) -> &str {
        self.filename
    }
}

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

pub enum Output {
    Llvm(String),
    Asm(String),
}

pub enum Backend {
    Llvm,
    Kyir,
}

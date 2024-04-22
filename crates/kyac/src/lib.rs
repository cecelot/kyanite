mod ast;
mod backend;
mod builtins;
mod error;
mod macros;
mod parse;
mod pass;
mod token;

pub use crate::backend::kyir::arch::{ArchInstr, Frame};

pub mod arch {
    pub use crate::backend::kyir::arch::armv8a::Armv8a;
}

pub mod isa {
    pub use crate::backend::kyir::arch::armv8a::isa::A64;
}

#[cfg(feature = "llvm")]
use crate::backend::llvm;
use crate::{arch::Armv8a, backend::kyir, isa::A64, pass::SymbolTable};
use std::{fs::File, io::Read, path::Path};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn compile(source: &Source, backend: &Backend) -> Result<Output, PipelineError> {
    #[cfg(feature = "llvm")]
    let mut ast = ast::Ast::try_from(source)?;
    #[cfg(not(feature = "llvm"))]
    let ast = ast::Ast::try_from(source)?;
    let symbols = SymbolTable::from(&ast.nodes);
    let meta = pass::resolve_types(source, &symbols, &ast.nodes)
        .map_err(|e| PipelineError::TypeError(e.len()))?;
    match backend {
        #[cfg(feature = "llvm")]
        Backend::Llvm => Ok(Output::Llvm(
            llvm::Ir::build(&mut ast.nodes, symbols, meta).map_err(PipelineError::IrError)?,
        )),
        #[cfg(not(feature = "llvm"))]
        Backend::Llvm => panic!("LLVM backend not enabled"),
        Backend::Kyir => Ok(Output::Asm(kyir::asm::<A64, Armv8a>(
            &ast.nodes, &symbols, &meta, false,
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
    #[cfg(feature = "llvm")]
    #[error("(while building ir) {0}")]
    IrError(llvm::IrError),
    #[error("failed to compile (see output)")]
    CompileError(String),
}

pub enum Output {
    Llvm(String),
    Asm(String),
}

#[derive(Debug)]
pub enum Backend {
    Llvm,
    Kyir,
}

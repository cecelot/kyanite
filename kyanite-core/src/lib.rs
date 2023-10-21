use std::{collections::HashMap, fs::File, io::Read, path::Path};

use codegen::IrError;

use crate::{
    codegen::Ir,
    pass::{SymbolTable, TypeCheckPass},
};

pub use compile::Compile;

mod ast;
mod codegen;
mod compile;
mod kyir;
mod parse;
mod pass;
mod reporting;
pub mod subprocess;
mod token;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(thiserror::Error, Debug)]
pub enum PipelineError {
    #[error("file \"{0}\" does not exist")]
    FileNotFound(String),
    #[error("file is not valid UTF-8")]
    InvalidUtf8,
    #[error("(while lexing source) {0} error(s) encountered")]
    LexError(usize),
    #[error("(while parsing) {0} error(s) encountered")]
    ParseError(usize),
    #[error("(while type checking) {0} error(s) encountered")]
    TypeError(usize),
    #[error("(while building ir) {0}")]
    IrError(IrError),
    #[error("failed to compile (see output)")]
    CompileError(String),
}

#[derive(Debug)]
pub struct Program {
    filename: String,
    ir: String,
}

impl Program {
    pub fn from_file<P>(path: P) -> Result<Self, PipelineError>
    where
        P: AsRef<Path>,
    {
        let source = Source::new(path)?;
        let ast = ast::Ast::from_source(&source)?;
        Self::new(ast, source)
    }

    fn new(mut ast: ast::Ast, source: Source) -> Result<Self, PipelineError> {
        fn strip_prefix(filename: &str) -> String {
            let chars: Vec<_> = filename.chars().collect();
            let name: Vec<_> = chars.iter().rev().take_while(|&&c| c != '/').collect();
            name.iter().rev().copied().copied().collect()
        }
        let filename = strip_prefix(&source.filename);
        let symbols = SymbolTable::from(&ast.nodes);
        let mut accesses = HashMap::new();
        let mut pass = TypeCheckPass::new(&symbols, &mut accesses, source, &ast.nodes);
        pass.run().map_err(PipelineError::TypeError)?;
        Ok(Self {
            ir: Ir::from_ast(&mut ast.nodes, symbols, accesses).map_err(PipelineError::IrError)?,
            filename,
        })
    }
}

#[derive(Debug, Default, Clone)]
pub struct Source {
    filename: String,
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
            .to_string();
        let mut raw = String::new();
        // acceptable clone()
        let mut file =
            File::open(&path).map_err(|_| PipelineError::FileNotFound(filename.clone()))?;
        file.read_to_string(&mut raw)
            .map_err(|_| PipelineError::InvalidUtf8)?;
        Ok(Self {
            filename,
            chars: raw.chars().collect(),
            raw,
        })
    }

    pub fn in_memory(raw: String) -> Self {
        Self {
            filename: "in-memory.kya".into(),
            chars: raw.chars().collect(),
            raw,
        }
    }
}

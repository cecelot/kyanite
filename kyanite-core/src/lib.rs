use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
};

use codegen::{registers::Color, IrError};
use compile::{Kyir, LlvmIr};

use crate::{
    codegen::{
        liveness::{Graph, LiveRanges},
        Codegen, Ir,
    },
    kyir::{arch::amd64::Amd64, canon::Canon, Translator},
    pass::{SymbolTable, TypeCheckPass},
};

pub use compile::Compile;

mod ast;
mod codegen;
mod compile;
mod kyir;
mod macros;
mod parse;
mod pass;
mod reporting;
pub mod subprocess;
mod token;

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
    IrError(IrError),
    #[error("failed to compile (see output)")]
    CompileError(String),
}

pub struct Program<'a> {
    source: Source,
    writer: Option<&'a mut dyn Write>,
    llvm: bool,
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

impl<'a> Program<'a> {
    pub fn new(source: Source) -> Self {
        Self {
            source,
            writer: None,
            llvm: false,
        }
    }

    pub fn llvm(mut self, llvm: bool) -> Self {
        self.llvm = llvm;
        self
    }

    pub fn write_to(mut self, writer: &'a mut dyn Write) -> Self {
        self.writer = Some(writer);
        self
    }

    pub fn build(self) -> Result<String, PipelineError> {
        let mut stdout = std::io::stdout();
        let writer = self.writer.unwrap_or(&mut stdout);
        let mut ast = ast::Ast::from_source(&self.source)?;
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
            let ir = LlvmIr::from(
                Ir::from_ast(&mut ast.nodes, symbols, accesses).map_err(PipelineError::IrError)?,
            );
            ir.compile(&filename, writer)
        } else {
            let mut translator: Translator<Amd64> = Translator::new(&accesses, &symbols);
            let ir = translator.translate(&ast.nodes);
            let canon = Canon::new(ir);
            let ir = canon.canonicalize();
            let codegen: Codegen<Amd64> = Codegen::new(ir, translator.functions, &ast.nodes);
            let graph = Graph::from(&codegen.asm);
            let ranges = LiveRanges::from(graph);
            let ig = ranges.interference_graphs(codegen.asm.len());
            let color: Color<Amd64> = Color::new(ig);
            let colors = color.color(ranges);
            let asm = Kyir::from(codegen.format(colors));
            asm.compile(&filename, writer)
        }
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

    pub fn in_memory(raw: String) -> Self {
        Self {
            filename: "in-memory.kya",
            chars: raw.chars().collect(),
            raw,
        }
    }
}

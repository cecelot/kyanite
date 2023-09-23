use std::{fmt, fs::File};

use codegen::IrError;

use crate::{
    codegen::Ir,
    pass::{SymbolTable, TypeCheckPass},
    token::Token,
};

pub use compile::Compile;

mod ast;
pub mod cli;
mod codegen;
mod compile;
mod parse;
mod pass;
mod reporting;
mod token;

#[derive(thiserror::Error, Debug)]
pub enum PipelineError {
    #[error("File is not valid UTF-8")]
    InvalidUtf8,
    #[error("(while lexing source) {0} errors encountered")]
    LexError(usize),
    #[error("(while parsing) {0} errors encountered")]
    ParseError(usize),
    #[error("(while type checking) {0} errors encountered")]
    TypeError(usize),
    #[error("(while building ir) {0}")]
    IrError(IrError),
}

#[derive(Debug)]
pub struct Program {
    ast: ast::Ast,
    ir: String,
}

impl Program {
    fn new(ast: ast::Ast) -> Result<Self, PipelineError> {
        Ok(Self {
            ir: Ir::from_ast(&ast).map_err(PipelineError::IrError)?,
            ast,
        })
    }

    pub fn from_file(file: File) -> Result<Self, PipelineError> {
        let stream = token::TokenStream::new(file).map_err(|_| PipelineError::InvalidUtf8)?;
        // TODO: messy clone
        let raw = stream.raw.clone();
        let tokens: Vec<Token> = stream.collect();
        let errored = token::errors(&tokens);
        if errored > 0 {
            return Err(PipelineError::LexError(errored));
        }
        let ast = ast::Ast::new(raw, tokens)?;
        Self::new(ast)?.build()
    }

    pub fn from_string(str: String) -> Result<Self, PipelineError> {
        let stream = token::TokenStream::from(str);
        // TODO: messy clone
        let raw = stream.raw.clone();
        let tokens: Vec<Token> = stream.collect();
        let errored = token::errors(&tokens);
        if errored > 0 {
            return Err(PipelineError::LexError(errored));
        }
        let ast = ast::Ast::new(raw, tokens)?;
        Self::new(ast)?.build()
    }

    fn build(self) -> Result<Self, PipelineError> {
        let symbols = SymbolTable::from(&self.ast.file);
        let mut pass = TypeCheckPass::new(symbols, &self.ast.file);
        let errors = pass.run();
        if errors > 0 {
            return Err(PipelineError::TypeError(errors));
        }
        Ok(self)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ast.file)
    }
}

pub fn run(exec: &str, args: &[&str]) -> (String, String) {
    let output = std::process::Command::new(exec)
        .args(args)
        .output()
        .unwrap();
    (
        std::str::from_utf8(&output.stdout).unwrap().to_owned()
            + std::str::from_utf8(&output.stderr).unwrap(),
        format!("{} {}", exec, args.join(" ")),
    )
}

use std::{fmt, fs::File};

use parse::ParseError;
use token::Token;

mod ast;
pub mod cli;
mod codegen;
mod parse;
mod pass;
mod token;

pub use codegen::Ir;
pub use pass::{SymbolTable, TypeCheckPass};

#[derive(thiserror::Error, Debug)]
pub enum PipelineError {
    #[error("File is not valid UTF-8")]
    InvalidUtf8,
    #[error("(while lexing source) {0} errors encountered")]
    LexError(usize),
    #[error("{0}")]
    ParseError(ParseError),
}

#[derive(Debug)]
pub struct Program {
    pub ast: ast::Ast,
}

impl Program {
    pub fn from_file(file: File) -> Result<Self, PipelineError> {
        let tokens: Vec<Token> = match token::TokenStream::new(file) {
            Ok(tokens) => tokens.collect(),
            Err(_) => return Err(PipelineError::InvalidUtf8),
        };
        let errored = token::errors(&tokens);
        if errored > 0 {
            return Err(PipelineError::LexError(errored));
        }
        let ast = ast::Ast::new(tokens).map_err(PipelineError::ParseError)?;
        Ok(Self { ast })
    }

    pub fn from_string(str: String) -> Result<Self, PipelineError> {
        let tokens: Vec<Token> = token::TokenStream::from(str).collect();
        let errored = token::errors(&tokens);
        if errored > 0 {
            return Err(PipelineError::LexError(errored));
        }
        let ast = ast::Ast::new(tokens).map_err(PipelineError::ParseError)?;
        Ok(Self { ast })
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ast.file)
    }
}

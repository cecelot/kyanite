use std::{fmt, fs::File};

use token::{errored, Token};

mod ast;
pub mod cli;
mod parse;
mod symbols;
mod token;

#[derive(Debug)]
pub struct Program {
    ast: ast::Ast,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ast.root)
    }
}

impl From<File> for Program {
    fn from(file: File) -> Self {
        let tokens: Vec<Token> = match token::TokenStream::new(file) {
            Ok(stream) => stream.collect(),
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        };
        if errored(&tokens) {
            std::process::exit(1);
        }
        let ast = ast::Ast::from(tokens);
        Self { ast }
    }
}

impl From<String> for Program {
    fn from(s: String) -> Self {
        let tokens: Vec<Token> = token::TokenStream::from(s).collect();
        if errored(&tokens) {
            std::process::exit(1);
        }
        let ast = ast::Ast::from(tokens);
        Self { ast }
    }
}

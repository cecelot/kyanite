#![allow(dead_code)]
use std::{error::Error, fs::File};

use token::{errored, Token, TokenStream};

use crate::ast::Ast;

mod ast;
mod parse;
mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let f = File::open("examples/hello.kya")?;
    let stream = TokenStream::new(f)?;
    let tokens: Vec<Token> = stream.collect();
    if !errored(&tokens) {
        // println!(
        //     "{:?}",
        //     tokens
        //         .iter()
        //         .map(|t| format!("{:?}", t.kind))
        //         .collect::<Vec<String>>()
        // );
        let ast = Ast::new(tokens)?;
        println!("{:?}", ast);
    }

    Ok(())
}

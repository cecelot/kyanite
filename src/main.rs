use std::{error::Error, fs::File};

use token::errored;

use crate::token::{Token, TokenStream};

mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let f = File::open("examples/hello.kya")?;
    let stream = TokenStream::new(f)?;
    let tokens: Vec<Token> = stream.collect();
    if !errored(&tokens) {
        println!(
            "{:?}",
            tokens
                .iter()
                .map(|t| format!("{:?}", t.kind))
                .collect::<Vec<String>>()
        );
    }

    Ok(())
}

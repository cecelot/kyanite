use std::{error::Error, fs::File};

use kyanite::Program;

fn main() -> Result<(), Box<dyn Error>> {
    let cli = kyanite::cli::init();
    match cli.command {
        kyanite::cli::Commands::Eval { source } => {
            let program = Program::from(source);
            println!("\n\n{}", program);
        }
        kyanite::cli::Commands::Run { path } => {
            let program = Program::from(File::open(path)?);
            println!("\n\n{}", program);
        }
    }

    Ok(())
}

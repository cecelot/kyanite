use std::fs::File;

use colored::Colorize;
use kyanite::{Compile, Program};

fn main() {
    let cli = kyanite::cli::init();
    match cli.command {
        kyanite::cli::Commands::Eval { source } => {
            let _ = match Program::from_string(source) {
                Ok(program) => program,
                Err(e) => {
                    println!("{}: {}", "error".bold().red(), e);
                    return;
                }
            };
        }
        kyanite::cli::Commands::Run { path } => {
            if let Ok(file) = File::open(&path) {
                let program = match Program::from_file(file) {
                    Ok(program) => program,
                    Err(e) => {
                        println!("{}: {}", "error".bold().red(), e);
                        return;
                    }
                };
                let entrypoint = program.compile();
                println!("{} `./{}`", "Running".bold().green(), entrypoint);
                print!("{}", kyanite::run(&format!("./{entrypoint}"), &[]).0);
            } else {
                println!("{}: file {path:?} does not exist", "error".bold().red());
            }
        }
    }
}

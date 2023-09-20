use std::{error::Error, fs::File};

use kyanite::{Ir, Program, SymbolTable, TypeCheckPass};
use log::{info, LevelFilter};

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::builder()
        .filter_level(LevelFilter::Info)
        .format_timestamp(None)
        .init();

    let cli = kyanite::cli::init();
    match cli.command {
        kyanite::cli::Commands::Eval { source: _ } => todo!(),
        kyanite::cli::Commands::Run { path } => {
            let file = Program::from(File::open(path)?).ast.file;
            let symbols = SymbolTable::from(&file);
            let mut pass = TypeCheckPass::new(symbols, &file);
            pass.run();
            Ir::build(&file).unwrap();

            let build = if cfg!(debug_assertions) {
                "target/debug"
            } else {
                "target/release"
            };

            info!(
                "llc -filetype=obj -o main.o out.ll ->\n{}",
                run("llc", &["-filetype=obj", "-o", "main.o", "out.ll"])
            );
            info!(
                "clang main.o -o main -L{} -lkyanite_builtins ->\n{}",
                build,
                run(
                    "clang",
                    &["main.o", "-o", "main", "-L", build, "-lkyanite_builtins"]
                )
            );
            info!("./main ->\n{}", run("./main", &[]));
        }
    }

    Ok(())
}

fn run(exec: &str, args: &[&str]) -> String {
    let output = std::process::Command::new(exec)
        .args(args)
        .output()
        .unwrap();
    std::str::from_utf8(&output.stdout).unwrap().to_owned()
        + std::str::from_utf8(&output.stderr).unwrap()
}

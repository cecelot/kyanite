use std::fs::File;

use kyanite::{Ir, Program, SymbolTable, TypeCheckPass};
use log::{error, info, LevelFilter};

fn main() {
    env_logger::builder()
        .filter_level(LevelFilter::Info)
        .format_timestamp(None)
        .init();

    let cli = kyanite::cli::init();
    match cli.command {
        kyanite::cli::Commands::Eval { source } => {
            let _ = match Program::from_string(source) {
                Ok(program) => program.ast.file,
                Err(e) => {
                    error!("{}", e);
                    return;
                }
            };
        }
        kyanite::cli::Commands::Run { path } => {
            if let Ok(file) = File::open(&path) {
                let file = match Program::from_file(file) {
                    Ok(program) => program.ast.file,
                    Err(e) => {
                        error!("{}", e);
                        return;
                    }
                };
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
            } else {
                error!("File {path:?} does not exist");
            }
        }
    }
}

fn run(exec: &str, args: &[&str]) -> String {
    let output = std::process::Command::new(exec)
        .args(args)
        .output()
        .unwrap();
    std::str::from_utf8(&output.stdout).unwrap().to_owned()
        + std::str::from_utf8(&output.stderr).unwrap()
}

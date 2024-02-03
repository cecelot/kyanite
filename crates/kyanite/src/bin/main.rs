use colored::Colorize;
use kyac::{Amd64, Backend, Output, Source};
use kyanite::{asm, include_dir, installed, llvm, Commands};
use std::{
    fmt,
    io::{BufRead, BufReader, Write},
    process::Stdio,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = kyanite::init();
    if !(cli.kyir || installed("llc") && installed("clang")) {
        return Ok(());
    }
    let backend = if cli.kyir {
        Backend::Kyir
    } else {
        Backend::Llvm
    };
    let cli = kyanite::init();
    let mut stdout = std::io::stdout();
    match cli.command {
        Commands::Run { path } => {
            let source = Source::new(path).unwrap_or_else(exit);
            let output = kyac::compile(&source, &backend).unwrap_or_else(exit);
            let filename = kyanite::filename(&source);
            let exe = match &output {
                Output::Llvm(ir) => llvm::compile(ir, &filename, &mut stdout).unwrap_or_else(exit),
                Output::Asm(asm) => {
                    asm::compile::<Amd64>(asm, &filename, &mut stdout).unwrap_or_else(exit)
                }
            };
            std::env::set_var("DYLD_LIBRARY_PATH", include_dir(&backend, None));
            writeln!(&mut stdout, "{} `./{}`", "Running".bold().green(), exe).unwrap();
            let child = std::process::Command::new(exe)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap_or_else(exit);
            let reader = BufReader::new(child.stdout.unwrap());
            for line in reader.lines() {
                writeln!(&mut stdout, "{}", line.unwrap()).unwrap();
            }
            Ok(())
        }
        Commands::Build { path: _ } => todo!(),
        Commands::Version => {
            println!("kyanite {} (kyac {})", kyanite::VERSION, kyac::VERSION);
            Ok(())
        }
    }
}

#[allow(clippy::needless_pass_by_value)]
fn exit<E: fmt::Display, R>(e: E) -> R {
    let mut stdout = std::io::stdout();
    writeln!(stdout, "{}: {}", "error".bold().red(), e).unwrap();
    std::process::exit(1);
}

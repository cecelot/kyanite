use colored::Colorize;
use kyac::{Amd64, Backend, Output, Source};
use kyanite::{asm, installed, llvm, Commands};
use std::io::Write;

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
            let source = Source::new(path).unwrap_or_else(|e| {
                writeln!(&mut stdout, "{}: {}", "error".bold().red(), e).unwrap();
                std::process::exit(1);
            });
            let output = kyac::compile(&source, &backend).unwrap_or_else(|e| {
                writeln!(&mut stdout, "{}: {}", "error".bold().red(), e).unwrap();
                std::process::exit(1);
            });
            let filename = kyanite::filename(&source);
            let exe = match &output {
                Output::Llvm(ir) => llvm::compile(ir, &filename, &mut stdout).unwrap_or_else(|e| {
                    writeln!(&mut stdout, "{}: {}", "error".bold().red(), e).unwrap();
                    std::process::exit(1);
                }),
                Output::Asm(asm) => asm::compile::<Amd64>(asm, &filename, &mut stdout)
                    .unwrap_or_else(|e| {
                        writeln!(&mut stdout, "{}: {}", "error".bold().red(), e).unwrap();
                        std::process::exit(1);
                    }),
            };
            writeln!(&mut stdout, "{} `./{}`", "Running".bold().green(), exe).unwrap();
            if let Output::Llvm(_) = output {
                let output = subprocess::exec(&exe, &[]).output;
                write!(stdout, "{output}")?;
            } else {
                let output = subprocess::exec(
                    "orb",
                    &[
                        &format!("LD_LIBRARY_PATH={}", kyanite::include_dir()),
                        &format!("./{exe}"),
                    ],
                )
                .output;
                write!(stdout, "{output}")?;
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

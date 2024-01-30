use colored::Colorize;
use kyac::Program;
use kyanite::{Backend, Commands};
use std::io::Write;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = kyanite::init();
    let backend = cli.backend.unwrap_or(Backend::Llvm);
    let llvm = backend == Backend::Llvm;
    if llvm && !(installed("llc") && installed("clang")) {
        return Ok(());
    }
    let cli = kyanite::init();
    let mut stdout = std::io::stdout();
    match cli.command {
        Commands::Run { path } => {
            let program = Program::try_from(path).unwrap_or_else(|e| {
                writeln!(&mut stdout, "{}: {}", "error".bold().red(), e).unwrap();
                std::process::exit(1);
            });
            let exe = program.llvm(llvm).build().unwrap_or_else(|e| {
                writeln!(&mut stdout, "{}: {}", "error".bold().red(), e).unwrap();
                std::process::exit(1);
            });
            if exe.is_empty() {
                writeln!(
                    &mut stdout,
                    "{}: executable not found",
                    "warning".bold().yellow()
                )
                .unwrap();
                Ok(())
            } else {
                writeln!(&mut stdout, "{} `./{exe}`", "Running".bold().green()).unwrap();
                let output = subprocess::exec(
                    "orb",
                    &[
                        &format!("LD_LIBRARY_PATH={}", kyac::compile::include_dir()),
                        &format!("./{exe}"),
                    ],
                )
                .output;
                Ok(write!(stdout, "{output}")?)
            }
        }
        Commands::Build { path: _ } => todo!(),
        Commands::Version => {
            println!("kyanite {} (kyac {})", kyanite::VERSION, kyac::VERSION);
            Ok(())
        }
    }
}

fn installed(s: &str) -> bool {
    if which::which(s).is_err() {
        println!(
            "{}: {} not found in PATH; try installing LLVM",
            "error".bold().red(),
            s
        );
        false
    } else {
        true
    }
}

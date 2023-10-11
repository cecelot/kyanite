use colored::Colorize;
use kyanite::Program;
use kyanite_cli::Commands;

fn main() {
    // Check for LLVM
    if !installed("llc") || !installed("clang") {
        return;
    }

    let cli = kyanite_cli::init();
    let stdout = std::io::stdout();
    let res = match cli.command {
        Commands::Run { path } => kyanite_cli::run(Program::from_file(path), stdout),
        Commands::Build { path } => kyanite_cli::build(Program::from_file(path), stdout),
        Commands::Version => {
            println!(
                "kyanite {} (kyac {})",
                kyanite_cli::VERSION,
                kyanite::VERSION
            );
            Ok(())
        }
    };
    match res {
        Ok(_) => (),
        Err(e) => println!("{}: {}", "error".bold().red(), e),
    }
}

fn installed(s: &str) -> bool {
    match which::which(s) {
        Ok(_) => true,
        Err(_) => {
            println!(
                "{}: {} not found in PATH; try installing LLVM",
                "error".bold().red(),
                s
            );
            false
        }
    }
}

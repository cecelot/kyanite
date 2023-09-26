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
        Commands::Eval { text } => kyanite_cli::run(Program::from_string(text), stdout),
        Commands::Run { path } => kyanite_cli::run(Program::from_file(path), stdout),
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

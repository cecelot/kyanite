use colored::Colorize;
use kyanite::Program;
use kyanite_cli::Commands;

fn main() {
    match which::which("llc") {
        Ok(_) => (),
        Err(_) => {
            println!(
                "{}: llc not found in PATH; try installing LLVM",
                "error".bold().red(),
            );
            return;
        }
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

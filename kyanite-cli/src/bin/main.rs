use colored::Colorize;
use kyanite::Program;
use kyanite_cli::Commands;

fn main() {
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

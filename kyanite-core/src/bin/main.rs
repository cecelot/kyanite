use colored::Colorize;
use kyanite::{cli::Commands, Compile, PipelineError, Program};

fn handle(res: Result<Program, PipelineError>) {
    let program = match res {
        Ok(program) => program,
        Err(e) => {
            println!("{}: {}", "error".bold().red(), e);
            return;
        }
    };
    let entrypoint = program.compile();
    println!("{} `./{}`", "Running".bold().green(), entrypoint);
    print!("{}", kyanite::run(&format!("./{entrypoint}"), &[]).0);
}

fn main() {
    let cli = kyanite::cli::init();
    match cli.command {
        Commands::Eval { text } => handle(Program::from_string(text)),
        Commands::Run { path } => handle(Program::from_file(path)),
    }
}

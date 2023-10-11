use clap::{Parser, Subcommand};
use colored::Colorize;
use std::{io::Write, path::PathBuf};

use kyanite::{Compile, PipelineError, Program};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The kyanite CLI.
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Runs a .kya file
    Run {
        /// The path to the .kya file
        path: PathBuf,
    },
    /// Builds a .kya file
    Build {
        /// The path to the .kya file
        path: PathBuf,
    },
    /// Prints the kyanite version
    Version,
}

/// Parses the command-line args and configures the logging level.
pub fn init() -> Cli {
    Cli::parse()
}

pub fn run(
    res: Result<Program, PipelineError>,
    mut writer: impl Write,
) -> Result<(), PipelineError> {
    let program = match res {
        Ok(program) => program,
        Err(e) => {
            println!("{}: {}", "error".bold().red(), e);
            return Ok(());
        }
    };
    let entrypoint = program.compile(&mut writer)?;
    writeln!(writer, "{} `./{}`", "Running".bold().green(), entrypoint).unwrap();
    write!(
        writer,
        "{}",
        kyanite::subprocess::exec(&format!("./{entrypoint}"), &[]).output
    )
    .unwrap();

    Ok(())
}

pub fn build(
    res: Result<Program, PipelineError>,
    mut writer: impl Write,
) -> Result<(), PipelineError> {
    let program = match res {
        Ok(program) => program,
        Err(e) => {
            println!("{}: {}", "error".bold().red(), e);
            return Ok(());
        }
    };
    let entrypoint = program.compile(&mut writer)?;
    writeln!(writer, "{} `./{}`", "Built".bold().green(), entrypoint).unwrap();

    Ok(())
}

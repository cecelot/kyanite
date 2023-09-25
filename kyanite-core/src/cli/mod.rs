use clap::{Parser, Subcommand};
use std::path::PathBuf;

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
    /// Evaluates kyanite code
    Eval {
        /// The source text to evaluate
        text: String,
    },
    /// Runs a .kya file
    Run {
        /// The path to the .kya file
        path: PathBuf,
    },
}

/// Parses the command-line args and configures the logging level.
#[must_use]
pub fn init() -> Cli {
    Cli::parse()
}

use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The kyanite CLI.
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
    pub backend: Option<Backend>,
}

#[derive(Clone, PartialEq, Eq, ValueEnum)]
pub enum Backend {
    LLVM,
    Kyir,
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

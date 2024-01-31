pub mod asm;
pub mod llvm;

use clap::{Parser, Subcommand};
use colored::Colorize;
use kyac::Source;
use std::path::PathBuf;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The kyanite CLI.
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
    #[arg(short, long)]
    pub kyir: bool,
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
#[must_use]
pub fn init() -> Cli {
    Cli::parse()
}

#[must_use]
pub fn installed(s: &str) -> bool {
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

#[must_use]
pub fn include_dir() -> String {
    let build = if cfg!(debug_assertions) {
        "target/debug"
    } else {
        "target/release"
    };
    let dir = env!("CARGO_MANIFEST_DIR");
    let dir = &dir[0..dir.len() - 14];
    let include = &format!("{dir}{build}");
    include.into()
}

#[must_use]
fn release_flag() -> &'static str {
    if cfg!(debug_assertions) {
        "--"
    } else {
        "--release"
    }
}

#[must_use]
pub fn filename(source: &Source) -> String {
    let name: Vec<_> = source
        .filename()
        .chars()
        .rev()
        .take_while(|&c| c != '/')
        .collect();
    name.iter().rev().collect()
}

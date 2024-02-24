pub mod asm;
pub mod llvm;

use clap::{Parser, Subcommand};
use colored::Colorize;
use fern::colors::{Color, ColoredLevelConfig};
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
    #[arg(short, long, global = true)]
    pub kyir: bool,
    #[arg(short, long, action = clap::ArgAction::Count, global = true)]
    pub verbose: u8,
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
    /// Cleans the target directory
    Clean,
    /// Prints the kyanite version
    Version,
}

/// Parses the command-line args and configures the logging level.
#[must_use]
pub fn cli() -> Cli {
    Cli::parse()
}

#[must_use]
pub fn installed(s: &str, msg: &str) -> bool {
    if which::which(s).is_err() {
        log::error!("{s} not found in PATH; {msg}");
        false
    } else {
        true
    }
}

#[must_use]
pub fn include_dir() -> String {
    let default = || -> String {
        let build = if cfg!(debug_assertions) {
            "target/debug"
        } else {
            "target/release"
        };
        let dir = env!("CARGO_MANIFEST_DIR");
        let dir = &dir[0..dir.len() - 14];
        let include = &format!("{dir}{build}");
        include.into()
    };
    std::env::var("KYANITE_RUNTIME_LIB").unwrap_or(default())
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

pub fn dylib(dir: &str) {
    let res = subprocess::exec("file", &[&format!("{dir}/libkyanite_runtime.dylib")]);
    log::debug!("libkyanite_runtime.dylib: {}", res.output.trim());
}

pub fn init_logger(verbosity: u8) -> Result<(), fern::InitError> {
    let level = match verbosity {
        0 => log::LevelFilter::Warn,
        1 => log::LevelFilter::Info,
        2 => log::LevelFilter::Debug,
        3.. => log::LevelFilter::Trace,
    };
    let colors = ColoredLevelConfig::new()
        .error(Color::Red)
        .warn(Color::Yellow)
        .info(Color::Green)
        .debug(Color::Magenta)
        .trace(Color::Cyan);
    fern::Dispatch::new()
        .format(move |out, message, record| {
            out.finish(format_args!(
                "{}: {}",
                format!("{}", colors.color(record.level()))
                    .to_lowercase()
                    .bold(),
                message
            ));
        })
        .level(level)
        .chain(std::io::stdout())
        .apply()?;
    // Enable GC logging if verbosity is at trace level
    if verbosity > 2 {
        std::env::set_var("KYANITE_LOG_GC", "1");
    }
    Ok(())
}

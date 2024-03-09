pub mod asm;
pub mod llvm;
pub mod subprocess;

use clap::{Parser, Subcommand};
use colored::Colorize;
use fern::colors::{Color, ColoredLevelConfig};
use kyac::{arch::Armv8a, isa::A64, Backend, Output, Source};
use std::{fmt, fs::File, path::PathBuf};
use tempfile::TempDir;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The kyanite CLI.
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
    #[arg(short, long, global = true)]
    /// Whether to use the LLVM backend instead of the kyir backend
    pub llvm: bool,
    #[arg(short, long, action = clap::ArgAction::Count, global = true)]
    /// The verbosity level (0-3)
    pub verbose: u8,
    #[arg(short, long, global = true)]
    /// Whether to run the garbage collector before every allocation (for debugging purposes)
    pub gc_always: bool,
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
pub fn cli() -> Cli {
    Cli::parse()
}

pub fn build(path: PathBuf, dir: &TempDir, backend: &Backend) -> String {
    log::info!("compiling `{}`", path.to_string_lossy());
    let source = Source::new(path).unwrap_or_else(fatal);
    let output = kyac::compile(&source, backend).unwrap_or_else(fatal);
    let filename = filename(&source);
    let exe = match &output {
        Output::Llvm(ir) => llvm::compile(ir, dir, &filename).unwrap_or_else(fatal),
        Output::Asm(asm) => asm::compile::<A64, Armv8a>(asm, dir, &filename).unwrap_or_else(fatal),
    };
    copy_exe(&filename, &exe).unwrap_or_else(fatal)
}

fn copy_exe(filename: &str, exe: &str) -> Result<String, Box<dyn std::error::Error>> {
    let to = filename.replace(".kya", "");
    File::create(&to)?;
    std::fs::copy(exe, &to)?;
    Ok(to)
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

pub fn fatal<E: fmt::Display, R>(e: E) -> R {
    log::error!("{}", e);
    std::process::exit(1);
}

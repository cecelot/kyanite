use kyac::Backend;
use kyanite::{installed, Commands};
use std::{
    io::{BufRead, BufReader, Write},
    process::Stdio,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = kyanite::cli();
    kyanite::init_logger(cli.verbose)?;
    if cli.llvm
        && !(installed("llc", "try installing LLVM") && installed("clang", "try installing LLVM"))
    {
        return Ok(());
    }
    let backend = if cli.llvm {
        Backend::Llvm
    } else {
        Backend::Kyir
    };
    log::debug!("using backend: {:?}", backend);
    let cli = kyanite::cli();
    match cli.command {
        Commands::Run { path } => {
            let exe = kyanite::build(path, &backend, cli.retain_artifacts);
            log::info!("running ./{exe}");
            let child = std::process::Command::new(format!("./{exe}"))
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap_or_else(kyanite::fatal);
            let reader = BufReader::new(child.stdout.unwrap());
            let mut stdout = std::io::stdout();
            for line in reader.lines() {
                writeln!(&mut stdout, "{}", line.unwrap()).unwrap();
            }
            Ok(())
        }
        Commands::Clean => {
            if let Err(e) = std::fs::remove_dir_all("kya-dist") {
                log::warn!("failed to remove kya-dist: {e}");
            }
            Ok(())
        }
        Commands::Build { path } => {
            let exe = kyanite::build(path, &backend, cli.retain_artifacts);
            log::info!("built ./{exe}");
            Ok(())
        }
        Commands::Version => {
            println!("kyanite {} (kyac {})", kyanite::VERSION, kyac::VERSION);
            Ok(())
        }
    }
}

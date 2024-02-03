use kyac::{Amd64, Backend, Output, Source};
use kyanite::{asm, include_dir, installed, llvm, Commands};
use std::{
    fmt,
    io::{BufRead, BufReader, Write},
    process::Stdio,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = kyanite::cli();
    kyanite::init_logger(cli.verbose)?;
    if !(cli.kyir
        || installed("llc", "try installing LLVM") && installed("clang", "try installing LLVM"))
    {
        return Ok(());
    }
    let backend = if cli.kyir {
        Backend::Kyir
    } else {
        Backend::Llvm
    };
    log::debug!("using backend: {:?}", backend);
    let cli = kyanite::cli();
    log::debug!("DYLD_LIBRARY_PATH: `{}`", include_dir(&backend, None));
    std::env::set_var("DYLD_LIBRARY_PATH", include_dir(&backend, None));
    match cli.command {
        Commands::Run { path } => {
            let source = Source::new(path).unwrap_or_else(exit);
            let output = kyac::compile(&source, &backend).unwrap_or_else(exit);
            let filename = kyanite::filename(&source);
            let exe = match &output {
                Output::Llvm(ir) => llvm::compile(ir, &filename).unwrap_or_else(exit),
                Output::Asm(asm) => asm::compile::<Amd64>(asm, &filename).unwrap_or_else(exit),
            };
            log::info!("running `./{}`", exe);
            let child = std::process::Command::new(exe)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap_or_else(exit);
            let reader = BufReader::new(child.stdout.unwrap());
            let mut stdout = std::io::stdout();
            for line in reader.lines() {
                writeln!(&mut stdout, "{}", line.unwrap()).unwrap();
            }
            Ok(())
        }
        Commands::Build { path: _ } => todo!(),
        Commands::Version => {
            println!("kyanite {} (kyac {})", kyanite::VERSION, kyac::VERSION);
            Ok(())
        }
    }
}

#[allow(clippy::needless_pass_by_value)]
fn exit<E: fmt::Display, R>(e: E) -> R {
    log::error!("{}", e);
    std::process::exit(1);
}

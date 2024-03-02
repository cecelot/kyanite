use crate::include_dir;
use kyac::{ArchInstr, Frame, PipelineError};
use std::{fs::File, io::Write};

pub fn compile<I: ArchInstr, F: Frame<I>>(
    instrs: &str,
    filename: &str,
) -> Result<String, PipelineError> {
    let _ = std::fs::create_dir("kya-dist");
    let asm = &format!("kya-dist/{filename}.s");
    let exe = &format!("kya-dist/{filename}");
    let mut file = File::create(asm).expect("well-formed file structure");
    write!(file, "{}{}", F::header(), instrs).unwrap();
    subprocess::handle(subprocess::exec(
        "clang",
        &[
            asm,
            "-o",
            exe,
            &format!("{}/libkyanite_runtime.a", include_dir()),
        ],
    ))
    .map_err(PipelineError::CompileError)?;
    Ok(exe.into())
}

use crate::{include_dir, subprocess};
use kyac::{ArchInstr, Frame, PipelineError};
use std::{fs::File, io::Write};
use tempfile::TempDir;

pub fn compile<I: ArchInstr, F: Frame<I>>(
    instrs: &str,
    dir: &TempDir,
    filename: &str,
) -> Result<String, PipelineError> {
    let asm = dir.path().join(format!("{filename}.s"));
    let exe = dir.path().join(filename);
    let asm = asm.display().to_string();
    let exe = exe.display().to_string();
    let mut file = File::create(&asm).expect("well-formed file structure");
    write!(file, "{}{}", F::header(), instrs).unwrap();
    subprocess::handle(subprocess::exec(
        "clang",
        &[&asm, "-o", &exe, &format!("{}/libruntime.a", include_dir())],
    ))
    .map_err(PipelineError::CompileError)?;
    Ok(exe)
}

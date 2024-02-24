use crate::include_dir;
use kyac::{Frame, PipelineError};
use std::{fs::File, io::Write};

pub fn compile<F: Frame>(instrs: &str, filename: &str) -> Result<String, PipelineError> {
    let _ = std::fs::create_dir("kya-dist");
    let asm = &format!("kya-dist/{filename}.s");
    let exe = &format!("kya-dist/{filename}");
    let mut file = File::create(asm).expect("well-formed file structure");
    write!(file, "{}{}", F::header(), instrs).unwrap();
    crate::dylib(&include_dir());
    subprocess::handle(subprocess::exec(
        "clang",
        &[asm, "-o", exe, "-L", &include_dir(), "-lkyanite_runtime"],
    ))
    .map_err(PipelineError::CompileError)?;
    Ok(exe.into())
}

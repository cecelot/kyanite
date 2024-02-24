use crate::include_dir;
use kyac::PipelineError;
use std::{fs::File, io::Write};

pub fn compile(ir: &str, filename: &str) -> Result<String, PipelineError> {
    let _ = std::fs::create_dir("kya-dist");
    let path = &format!("kya-dist/{filename}.ll");
    let obj = &format!("kya-dist/{filename}.o");
    let exe = &format!("kya-dist/{filename}");
    let mut file = File::create(path).expect("well-formed file structure");
    write!(file, "{ir}").unwrap();
    crate::dylib(&include_dir());
    subprocess::handle(subprocess::exec("llc", &["-filetype=obj", "-o", obj, path]))
        .map_err(PipelineError::CompileError)?;
    subprocess::handle(subprocess::exec(
        "clang",
        &[obj, "-o", exe, "-L", &include_dir(), "-lkyanite_runtime"],
    ))
    .map_err(PipelineError::CompileError)?;
    Ok(exe.into())
}

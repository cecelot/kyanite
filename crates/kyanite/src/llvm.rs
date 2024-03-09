use crate::{include_dir, subprocess};
use kyac::PipelineError;
use std::{fs::File, io::Write};
use tempfile::TempDir;

pub fn compile(ir: &str, dir: &TempDir, filename: &str) -> Result<String, PipelineError> {
    let path = dir.path().join(format!("{filename}.ll"));
    let obj = dir.path().join(format!("{filename}.o"));
    let exe = dir.path().join(filename);
    let path = path.display().to_string();
    let obj = obj.display().to_string();
    let exe = exe.display().to_string();
    let mut file = File::create(&path).expect("well-formed file structure");
    write!(file, "{ir}").unwrap();
    subprocess::handle(subprocess::exec(
        "llc",
        &["-filetype=obj", "-o", &obj, &path],
    ))
    .map_err(PipelineError::CompileError)?;
    subprocess::handle(subprocess::exec(
        "clang",
        &[&obj, "-o", &exe, &format!("{}/libruntime.a", include_dir())],
    ))
    .map_err(PipelineError::CompileError)?;
    Ok(exe)
}

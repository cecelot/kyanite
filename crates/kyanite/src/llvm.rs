use crate::{include_dir, release_flag};
use kyac::PipelineError;
use std::{fs::File, io::Write};

pub fn compile(ir: &str, filename: &str, mut writer: impl Write) -> Result<String, PipelineError> {
    let _ = std::fs::create_dir("kya-dist");
    let path = &format!("kya-dist/{filename}.ll");
    let obj = &format!("kya-dist/{filename}.o");
    let exe = &format!("kya-dist/{filename}");
    let mut file = File::create(path).expect("well-formed file structure");
    write!(file, "{ir}").unwrap();
    subprocess::handle(
        "Finished",
        subprocess::exec(
            "cargo",
            &[
                "build",
                "--package",
                "builtins",
                release_flag(), // This must be last because with debug enabled, this will be treated as "--",
                                // which escapes any following arguments
            ],
        ),
        &mut writer,
    )
    .map_err(PipelineError::CompileError)?;
    subprocess::handle(
        "Finished",
        subprocess::exec("llc", &["-filetype=obj", "-o", obj, path]),
        &mut writer,
    )
    .map_err(PipelineError::CompileError)?;
    subprocess::handle(
        "Finished",
        subprocess::exec(
            "clang",
            &[obj, "-o", exe, "-L", &include_dir(), "-lkyanite_builtins"],
        ),
        &mut writer,
    )
    .map_err(PipelineError::CompileError)?;
    Ok(exe.into())
}

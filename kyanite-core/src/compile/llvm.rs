use crate::{compile::include_dir, subprocess, PipelineError};
use std::{fs::File, io::Write};

pub struct LlvmIr(pub String);

impl LlvmIr {
    pub fn compile(&self, filename: &str, mut writer: impl Write) -> Result<String, PipelineError> {
        let _ = std::fs::create_dir("kya-dist");
        let ir = &format!("kya-dist/{filename}.ll");
        let obj = &format!("kya-dist/{filename}.o");
        let exe = &format!("kya-dist/{filename}");
        let mut file = File::create(ir).expect("well-formed file structure");
        write!(file, "{}", self.0).unwrap();
        subprocess::handle(
            "Finished",
            subprocess::exec("llc", &["-filetype=obj", "-o", obj, ir]),
            &mut writer,
        )?;
        subprocess::handle(
            "Finished",
            subprocess::exec(
                "clang",
                &[obj, "-o", exe, "-L", &include_dir(), "-lkyanite_builtins"],
            ),
            &mut writer,
        )?;
        Ok(exe.into())
    }
}

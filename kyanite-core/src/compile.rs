use std::{fs::File, io::Write, path::Path};

use crate::{subprocess, PipelineError};

pub struct LlvmIr(String);

impl From<String> for LlvmIr {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub struct Kyir(String);

impl From<String> for Kyir {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub trait Compile {
    fn compile(&self, filename: &str, writer: impl Write) -> Result<String, PipelineError>;
}

impl Compile for LlvmIr {
    fn compile(&self, filename: &str, mut writer: impl Write) -> Result<String, PipelineError> {
        if !Path::new("kya-dist").exists() {
            let _ = std::fs::create_dir("kya-dist");
        }
        let ir = &format!("kya-dist/{}.ll", filename);
        let obj = &format!("kya-dist/{}.o", filename);
        let exe = &format!("kya-dist/{}", filename);
        let mut file = File::create(ir).expect("well-formed file structure");
        write!(file, "{}", self.0).unwrap();
        let build = if cfg!(debug_assertions) {
            "target/debug"
        } else {
            "target/release"
        };
        let dir = env!("CARGO_MANIFEST_DIR");
        let dir = &dir[0..dir.len() - 12];
        let include = &format!("{}{build}", dir);
        subprocess::handle(
            "Finished",
            subprocess::exec("llc", &["-filetype=obj", "-o", obj, ir]),
            &mut writer,
        )?;
        subprocess::handle(
            "Finished",
            subprocess::exec(
                "clang",
                &[obj, "-o", exe, "-L", include, "-lkyanite_builtins"],
            ),
            &mut writer,
        )?;
        Ok(exe.into())
    }
}

impl Compile for Kyir {
    fn compile(&self, filename: &str, mut writer: impl Write) -> Result<String, PipelineError> {
        if !Path::new("kya-dist").exists() {
            let _ = std::fs::create_dir("kya-dist");
        }
        let ir = &format!("kya-dist/{}.s", filename);
        let exe = &format!("kya-dist/{}", filename);
        let mut file = File::create(ir).expect("well-formed file structure");
        for instr in self.0.lines() {
            writeln!(file, "{}", instr).unwrap();
        }
        subprocess::handle(
            "Finished",
            subprocess::exec("clang", &[ir, "-o", exe]),
            &mut writer,
        )?;
        Ok(String::new())
    }
}

use std::{fs::File, io::Write, path::Path};

use crate::{kyir::arch::Frame, subprocess, PipelineError};

#[must_use]
pub fn include_dir() -> String {
    let build = if cfg!(debug_assertions) {
        "target/debug"
    } else {
        "target/release"
    };
    let dir = env!("CARGO_MANIFEST_DIR");
    let dir = &dir[0..dir.len() - 12];
    let include = &format!("{dir}{build}");
    include.into()
}

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
    fn compile<F: Frame>(
        &self,
        filename: &str,
        writer: impl Write,
    ) -> Result<String, PipelineError>;
}

impl Compile for LlvmIr {
    fn compile<F: Frame>(
        &self,
        filename: &str,
        mut writer: impl Write,
    ) -> Result<String, PipelineError> {
        if !Path::new("kya-dist").exists() {
            let _ = std::fs::create_dir("kya-dist");
        }
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

impl Compile for Kyir {
    fn compile<F: Frame>(
        &self,
        filename: &str,
        mut writer: impl Write,
    ) -> Result<String, PipelineError> {
        if !Path::new("kya-dist").exists() {
            let _ = std::fs::create_dir("kya-dist");
        }
        let ir = &format!("kya-dist/{filename}.s");
        let exe = &format!("kya-dist/{filename}");
        let mut file = File::create(ir).expect("well-formed file structure");
        writeln!(file, "{}", F::header()).unwrap();
        for instr in self.0.lines() {
            writeln!(file, "{instr}").unwrap();
        }
        subprocess::handle(
            "Finished",
            subprocess::exec("orb", &["cargo", "build", "--package", "kyanite_builtins"]),
            &mut writer,
        )?;
        subprocess::handle(
            "Finished",
            // Run via `OrbStack` to use x86 clang for testing purposes
            subprocess::exec(
                "orb",
                &[
                    "clang-15",
                    ir,
                    "-o",
                    exe,
                    "-L",
                    &include_dir(),
                    "-lkyanite_builtins",
                ],
            ),
            &mut writer,
        )?;
        Ok(exe.into())
    }
}

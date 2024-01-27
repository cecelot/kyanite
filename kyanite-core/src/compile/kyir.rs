use crate::{
    backend::kyir::arch::Frame,
    compile::{include_dir, release_flag},
    subprocess, PipelineError,
};
use std::{fs::File, io::Write};

pub struct Kyir(pub String);

impl Kyir {
    pub fn compile<F: Frame>(
        &self,
        filename: &str,
        mut writer: impl Write,
    ) -> Result<String, PipelineError> {
        let _ = std::fs::create_dir("kya-dist");
        let ir = &format!("kya-dist/{filename}.s");
        let exe = &format!("kya-dist/{filename}");
        let mut file = File::create(ir).expect("well-formed file structure");
        writeln!(file, "{}", F::header()).unwrap();
        for instr in self.0.lines() {
            writeln!(file, "{instr}").unwrap();
        }
        subprocess::handle(
            "Finished",
            subprocess::exec(
                "orb",
                &[
                    "cargo",
                    "build",
                    "--package",
                    "kyanite_builtins",
                    release_flag(), // This must be last because with debug enabled, this will be treated as "--",
                                    // which escapes any following arguments
                ],
            ),
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

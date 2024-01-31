use crate::{include_dir, release_flag};
use kyac::{Frame, PipelineError};
use std::{fs::File, io::Write};

pub fn compile<F: Frame>(
    asm: &str,
    filename: &str,
    mut writer: impl Write,
) -> Result<String, PipelineError> {
    let _ = std::fs::create_dir("kya-dist");
    let ir = &format!("kya-dist/{filename}.s");
    let exe = &format!("kya-dist/{filename}");
    let mut file = File::create(ir).expect("well-formed file structure");
    writeln!(file, "{}", F::header()).unwrap();
    for instr in asm.lines() {
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
    )
    .map_err(PipelineError::CompileError)?;
    Ok(exe.into())
}

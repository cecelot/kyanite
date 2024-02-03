use crate::include_dir;
use kyac::{Backend, Frame, PipelineError};
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
            // We need to run a bash shell because otherwise Zig complains that -target is an unknown Clang option
            // for some reason.
            "bash",
            &[
                "-c",
                &format!(
                    "zig cc {ir} -o {exe} -target x86_64-macos -L {} -lkyanite_builtins",
                    include_dir(&Backend::Kyir, Some("x86_64-apple-darwin"))
                ),
            ],
        ),
        &mut writer,
    )
    .map_err(PipelineError::CompileError)?;
    Ok(exe.into())
}

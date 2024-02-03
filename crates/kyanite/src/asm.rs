use crate::include_dir;
use kyac::{Backend, Frame, PipelineError};
use std::{fs::File, io::Write};

pub fn compile<F: Frame>(instrs: &str, filename: &str) -> Result<String, PipelineError> {
    let _ = std::fs::create_dir("kya-dist");
    let asm = &format!("kya-dist/{filename}.s");
    let exe = &format!("kya-dist/{filename}");
    let mut file = File::create(asm).expect("well-formed file structure");
    writeln!(file, "{}\n{}", F::header(), instrs).unwrap();
    crate::dylib(&include_dir(&Backend::Kyir, Some("x86_64-apple-darwin")));
    // We need to run a bash shell because otherwise Zig complains that -target is an unknown Clang option
    // for some reason.
    subprocess::handle(subprocess::exec(
        "bash",
        &[
            "-c",
            &format!(
                "zig cc {asm} -o {exe} -target x86_64-macos -L {} -lkyanite_builtins",
                include_dir(&Backend::Kyir, Some("x86_64-apple-darwin"))
            ),
        ],
    ))
    .map_err(PipelineError::CompileError)?;
    Ok(exe.into())
}

use std::{fs::File, io::Write, path::Path};

use crate::{subprocess, PipelineError, Program};

pub trait Compile {
    fn compile(&self, writer: impl Write) -> Result<String, PipelineError>;
}

impl Compile for Program {
    fn compile(&self, mut writer: impl Write) -> Result<String, PipelineError> {
        if !Path::new("kya-dist").exists() {
            std::fs::create_dir("kya-dist").expect("permission to create directories");
        }
        let ir = "kya-dist/main.ll";
        let obj = "kya-dist/main.o";
        let exe = "kya-dist/main";
        let mut file = File::create(ir).expect("well-formed file structure");
        write!(file, "{}", self.ir).unwrap();

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

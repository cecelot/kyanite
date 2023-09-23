use std::{fs::File, io::Write, path::Path};

use colored::Colorize;

use crate::Program;

pub trait Compile {
    fn compile(&self) -> String;
}

impl Compile for Program {
    fn compile(&self) -> String {
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

        let (_, llvm) = crate::run("llc", &["-filetype=obj", "-o", obj, ir]);
        println!("{} `{}`", "Finished".green().bold(), llvm);
        let (_, clang) = crate::run(
            "clang",
            &[obj, "-o", exe, "-L", build, "-lkyanite_builtins"],
        );
        println!("{} `{}`", "Finished".green().bold(), clang);
        exe.into()
    }
}

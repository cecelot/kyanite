use colored::Colorize;
use kyac::Program;
use std::io::Write;

macro_rules! assert_output {
    ($($path:expr => $name:ident),*) => {
        $(
            #[test]
            #[ignore]
            fn $name() -> Result<(), Box<dyn std::error::Error>> {
                let mut output = vec![];
                let program = Program::try_from($path).unwrap();
                let exe = program.llvm(true).write_to(&mut output).build().unwrap();
                writeln!(&mut output, "{} `./{exe}`", "Running".bold().green()).unwrap();
                let output = subprocess::exec(&format!("./{exe}"), &[]).output;
                insta::with_settings!({snapshot_path => "../snapshots"}, {
                    let lines: Vec<&str> = output.lines().collect();
                    insta::assert_yaml_snapshot!(lines);
                });
                Ok(())
            }
        )*
    }
}

assert_output! {
    "../examples/hello.kya" => hello,
    "../examples/exprs.kya" => exprs,
    "../examples/decls.kya" => decls,
    "../examples/functions.kya" => functions,
    "../examples/nested.kya" => nested
}

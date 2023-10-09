use kyanite::Program;

macro_rules! assert_output {
    ($($path:expr => $name:ident),*) => {
        $(
            #[test]
            fn $name() -> Result<(), Box<dyn std::error::Error>> {
                let mut output = vec![];
                kyanite_cli::run(Program::from_file($path), &mut output)?;
                let output = String::from_utf8(output)?;
                insta::with_settings!({snapshot_path => "../snapshots"}, {
                    let lines: Vec<&str> = output.lines().skip(3).collect();
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
    "../examples/functions.kya" => functions
}

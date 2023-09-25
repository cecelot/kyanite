use kyanite::Program;

#[test]
fn hello() -> Result<(), Box<dyn std::error::Error>> {
    let mut output = vec![];
    kyanite_cli::run(Program::from_file("../examples/hello.kya"), &mut output)?;

    insta::with_settings!({snapshot_path => "../snapshots"}, {
        insta::assert_display_snapshot!(String::from_utf8(output).unwrap());
    });

    Ok(())
}

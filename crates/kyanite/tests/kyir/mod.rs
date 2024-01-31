use kyac::{Amd64, Backend, Output, Source};
use subprocess::ProcessResult;

fn run(name: &str) -> Result<ProcessResult, Box<dyn std::error::Error>> {
    let source = Source::new(super::path(name)?)?;
    let Output::Asm(asm) = kyac::compile(&source, &Backend::Kyir)? else {
        unreachable!()
    };
    let exe = kyanite::asm::compile::<Amd64>(&asm, &kyanite::filename(&source), &mut vec![])?;
    let res = subprocess::exec(
        "orb",
        &[
            &format!("LD_LIBRARY_PATH={}", kyanite::include_dir()),
            &format!("./{exe}"),
        ],
    );
    Ok(res)
}

#[test]
fn simple() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/simple.kya")?;
    assert_eq!(res.output, "5\n");
    Ok(())
}

#[test]
fn conditions() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/conditions.kya")?;
    assert_eq!(res.output, "14\n24\n");
    Ok(())
}

#[test]
fn trivial_loop() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/trivial-loop.kya")?;
    assert_eq!(res.output, "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n");
    Ok(())
}

#[test]
fn nested_condition() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/nested-condition.kya")?;
    assert_eq!(
        res.output,
        "1\n2\n3\n4\n5\n6\n12\n7\n14\n8\n16\n9\n18\n10\n20\n"
    );
    Ok(())
}

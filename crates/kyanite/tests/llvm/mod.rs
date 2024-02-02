use kyac::{Backend, Output, Source};
use subprocess::ProcessResult;

fn run(name: &str) -> Result<ProcessResult, Box<dyn std::error::Error>> {
    let source = Source::new(super::path(name)?)?;
    let Output::Llvm(ir) = kyac::compile(&source, &Backend::Llvm)? else {
        unreachable!()
    };
    let exe = kyanite::llvm::compile(&ir, &kyanite::filename(&source), &mut vec![])?;
    let res = subprocess::exec(&exe, &[]);
    Ok(res)
}

#[test]
fn hello() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("llvm/hello.kya")?;
    assert_eq!(res.code, 0);
    assert_eq!(res.output, "Hello, world!\n");
    Ok(())
}

#[test]
fn exprs() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("llvm/exprs.kya")?;
    assert_eq!(res.code, 0);
    assert_eq!(res.output, "false\ntrue\nfalse\ntrue\ntrue\nfalse\n");
    Ok(())
}

#[test]
fn decls() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("llvm/decls.kya")?;
    assert_eq!(res.code, 0);
    assert_eq!(res.output, "5\n");
    Ok(())
}

#[test]
fn functions() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("llvm/functions.kya")?;
    assert_eq!(res.code, 0);
    assert_eq!(res.output, "5\n500\n17\n83\n");
    Ok(())
}

#[test]
fn nested() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("llvm/nested.kya")?;
    assert_eq!(res.code, 0);
    assert_eq!(res.output, "100\n500\n80\nHello\nWorld\n!\nWorld\n");
    Ok(())
}

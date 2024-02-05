use kyac::{Amd64, Backend, Output, Source};
use subprocess::ProcessResult;

fn run(name: &str) -> Result<ProcessResult, Box<dyn std::error::Error>> {
    let source = Source::new(super::path(name)?)?;
    let Output::Asm(asm) = kyac::compile(&source, &Backend::Kyir)? else {
        unreachable!()
    };
    let exe = kyanite::asm::compile::<Amd64>(&asm, &kyanite::filename(&source))?;
    let res = subprocess::exec(&exe, &[]);
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

#[test]
fn nested_loop() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/nested-loop.kya")?;
    assert_eq!(res.output, "5\n6\n7\n8\n9\n");
    Ok(())
}

#[test]
fn called_nested_loop() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/called-nested-loop.kya")?;
    assert_eq!(res.output, "5\n6\n7\n8\n9\n");
    Ok(())
}

#[test]
fn simple_record() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/simple-record.kya")?;
    assert_eq!(res.output, "3\n");
    Ok(())
}

#[test]
fn record_with_addition() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/record-with-addition.kya")?;
    assert_eq!(res.output, "7\n");
    Ok(())
}

#[test]
fn nested_records() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/nested-records.kya")?;
    assert_eq!(res.output, "1\n2\ntrue\n");
    Ok(())
}

#[test]
fn big_record() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/big-record.kya")?;
    assert_eq!(res.output, "1\n8\n19\n28\n");
    Ok(())
}

#[test]
fn record_access_in_condition() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/record-access-in-condition.kya")?;
    assert_eq!(res.output, "20\n50\n60\n");
    Ok(())
}

#[test]
fn factorial() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/factorial.kya")?;
    assert_eq!(res.output, "120\n3628800\n");
    Ok(())
}

#[test]
fn record_as_arg() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/record-as-arg.kya")?;
    assert_eq!(res.output, "1\n2\n18\n");
    Ok(())
}

#[test]
fn anon_calls() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/anon-calls.kya")?;
    assert_eq!(res.output, "38\n");
    Ok(())
}

#[test]
fn half_anon_call() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/half-anon-call.kya")?;
    assert_eq!(res.output, "38\n38\n38\n38\n");
    Ok(())
}

#[test]
fn fibonacci() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/fibonacci.kya")?;
    assert_eq!(res.output, "1\n1\n2\n3\n5\n8\n102334155\n");
    Ok(())
}

#[test]
fn early_return() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/early-return.kya")?;
    assert_eq!(res.output, "1\n0\n");
    Ok(())
}

#[test]
fn nested_early_return() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/nested-early-return.kya")?;
    assert_eq!(res.output, "1\n3\n5\n");
    Ok(())
}

#[test]
fn multi_depth_functions() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/multi-depth-functions.kya")?;
    assert_eq!(res.output, "7\n");
    Ok(())
}

#[test]
fn multi_depth_functions_with_conditions() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/multi-depth-functions-with-conditions.kya")?;
    assert_eq!(res.output, "3\n3\n6\n7\n2\n2\n0\n1\n6\n6\n12\n0\n");
    Ok(())
}

#[test]
fn simple_if() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/simple-if.kya")?;
    assert_eq!(res.output, "1\n");
    Ok(())
}

#[test]
fn record_return() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/record-return.kya")?;
    assert_eq!(res.output, "1\n2\n");
    Ok(())
}

#[test]
fn many_args() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/many-args.kya")?;
    assert_eq!(res.output, "1292\n1497\n101\n790\n982\nfalse\n16\n");
    Ok(())
}

#[test]
fn embedded_record_init() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/embedded-record-init.kya")?;
    assert_eq!(res.output, "7721\n14000\n");
    Ok(())
}

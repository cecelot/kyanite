use kyac::{arch::Armv8a, isa::A64, Backend, Output, Source};
use subprocess::ProcessResult;

fn run(name: &str) -> Result<ProcessResult, Box<dyn std::error::Error>> {
    let source = Source::new(super::path(name)?)?;
    let Output::Asm(asm) = kyac::compile(&source, &Backend::Kyir)? else {
        unreachable!()
    };
    let exe = kyanite::asm::compile::<A64, Armv8a>(&asm, &kyanite::filename(&source))?;
    std::env::set_var("KYANITE_GC_ALWAYS", "1");
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
fn simple_class() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/simple-class.kya")?;
    assert_eq!(res.output, "3\n");
    Ok(())
}

#[test]
fn class_with_addition() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/class-with-addition.kya")?;
    assert_eq!(res.output, "7\n");
    Ok(())
}

#[test]
fn class_composition() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/class-composition.kya")?;
    assert_eq!(res.output, "1\n2\ntrue\n");
    Ok(())
}

#[test]
fn large_class() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/large-class.kya")?;
    assert_eq!(res.output, "1\n8\n19\n28\n");
    Ok(())
}

#[test]
fn field_access_in_condition() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/field-access-in-condition.kya")?;
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
fn class_as_arg() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/class-as-arg.kya")?;
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
fn returns_class() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/returns-class.kya")?;
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
fn embedded_class_init() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/embedded-class-init.kya")?;
    assert_eq!(res.output, "7721\n14000\n");
    Ok(())
}

#[test]
fn complex_class() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/complex-class.kya")?;
    assert_eq!(res.output, "1\n2\n3\n4\n5\n15\n22\n16\n17\n77\n");
    Ok(())
}

#[test]
fn shadow() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/shadow.kya")?;
    assert_eq!(res.output, "17\n17\n");
    Ok(())
}

#[test]
fn more_classes() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/more-classes.kya")?;
    assert_eq!(res.output, "1\n8\n16\n9\n");
    Ok(())
}

#[test]
fn extreme_composition() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/extreme-composition.kya")?;
    assert_eq!(res.output, "5\n4\n3\n");
    Ok(())
}

#[test]
fn simple_for() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/simple-for.kya")?;
    assert_eq!(res.output, "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n");
    Ok(())
}

#[test]
fn for_loop() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/for-loop.kya")?;
    assert_eq!(res.output, "5\n6\n7\n8\n9\n1\n10\n1\n10\n5\n");
    Ok(())
}

#[test]
fn variable_while_loop() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/variable-while-loop.kya")?;
    assert_eq!(res.output, "5\n6\n7\n8\n9\n");
    Ok(())
}

#[test]
fn hello() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/hello.kya")?;
    assert_eq!(res.output, "Hello, world!\n");
    Ok(())
}

#[test]
fn misc_strings() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/misc-strings.kya")?;
    assert_eq!(res.output, "Hello, world!\nHello, world!\nbaz\nfoo\n");
    Ok(())
}

#[test]
fn gc_shared_child_field() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/gc-shared-child-field.kya")?;
    assert_eq!(res.output, "5\n5\n");
    Ok(())
}

#[test]
fn basic_methods() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/basic-methods.kya")?;
    assert_eq!(res.output, "14\n77\n999\nhello from `show()`\n");
    Ok(())
}

#[test]
fn nested_impl_call() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/nested-impl-call.kya")?;
    assert_eq!(res.output, "22\n");
    Ok(())
}

#[test]
fn anon_field_access() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/anon-field-access.kya")?;
    assert_eq!(res.output, "5\n7\n12\n");
    Ok(())
}

#[test]
fn anon_method_call() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/anon-method-call.kya")?;
    assert_eq!(res.output, "12\n");
    Ok(())
}

#[test]
fn nested_access_in_impl_call() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/nested-access-in-impl-call.kya")?;
    assert_eq!(res.output, "22\n22\n2\n144\n5\n7\n22\n77\n144\n");
    Ok(())
}

#[test]
fn field_mutation() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/field-mutation.kya")?;
    assert_eq!(res.output, "5\n6\n10\n106\n6\n");
    Ok(())
}

#[test]
fn reg_live_across_call() -> Result<(), Box<dyn std::error::Error>> {
    let res = run("kyir/reg-live-across-call.kya")?;
    assert_eq!(res.output, "4\n6\n");
    Ok(())
}

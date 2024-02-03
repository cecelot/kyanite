use crate::{
    backend::kyir::{opcode::Opcode, AsmInstr, Codegen, Instr},
    Frame,
};

// If any function calls another function, we need to avoid writing into the "red zone".
// more info: https://stackoverflow.com/a/63869171
pub fn run<F: Frame>(codegen: &mut Codegen<F>) {
    for (id, function) in &codegen.functions {
        if codegen.call.contains_key(id) {
            // Find the function and insert a `subq $16, %rsp` after the prologue.
            let start = codegen.asm.iter().position(|instr| {
                matches!(
                    &instr.inner,
                    Instr::Oper {
                        opcode: Opcode::Label(label),
                        ..
                    } if label == function.label()
                )
            });
            let fun = codegen
                .functions
                .get(&codegen.idents[function.label()])
                .unwrap();
            if let Some(start) = start {
                codegen.asm.insert(
                    start + fun.prologue().len() + 1,
                    AsmInstr::new(Instr::oper(
                        Opcode::Sub,
                        F::registers().stack.into(),
                        // Not sure if we actually need to align the stack to a multiple of 16 bytes, but we do it anyway.
                        format!("${}", next_multiple_of(fun.offset().abs(), 16)),
                        None,
                    )),
                );
            }
        }
    }
}

/// Stolen from <https://github.com/rust-lang/rust/issues/88581> until this is in stable.
fn next_multiple_of(n: i64, rhs: i64) -> i64 {
    if rhs == -1 {
        return n;
    }

    let r = n % rhs;
    let m = if (r > 0 && rhs < 0) || (r < 0 && rhs > 0) {
        r + rhs
    } else {
        r
    };

    if m == 0 {
        n
    } else {
        n + (rhs - m)
    }
}

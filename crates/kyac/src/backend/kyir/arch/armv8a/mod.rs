pub mod isa;

use crate::{
    ast::{node::FuncDecl, Type},
    backend::kyir::{
        arch::{Location, RegisterMap},
        ir::{BinOp, Binary, Const, Expr, Mem, Temp},
        Frame,
    },
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Armv8a {
    variables: HashMap<String, Variable>,
    formals: Vec<Formal>,
    label: String,
    offset: i64,
}

impl Frame<isa::A64> for Armv8a {
    fn new(func: &FuncDecl) -> Self {
        let r = Self::registers();
        assert!(func.params.len() <= 6);
        let mut variables = HashMap::new();
        let mut offset = 0;
        for (i, param) in func.params.iter().enumerate() {
            if i == 0 {
                offset -= i64::try_from(Self::word_size()).unwrap();
            }
            variables.insert(
                param.name.to_string(),
                Variable::new(
                    offset,
                    matches!(Type::from(&param.ty), Type::UserDefined(_)),
                ),
            );
            offset -= i64::try_from(Self::word_size()).unwrap();
        }
        Self {
            formals: func
                .params
                .iter()
                .enumerate()
                .map(|(i, _)| Formal::new(r.argument[i]))
                .collect(),
            label: func.name.to_string(),
            variables,
            offset,
        }
    }

    fn offset(&self) -> i64 {
        self.offset
    }

    fn label(&self) -> &String {
        &self.label
    }

    fn prefixed(call: &str) -> String {
        format!("_{call}")
    }

    fn get(&self, ident: &str) -> Expr {
        let offset = {
            let variable = self.variables.get(ident).unwrap_or_else(|| {
                println!("{:?}", self.variables);
                panic!("variable {} not found in frame {}", ident, self.label);
            });
            variable.offset
        };
        let r = Self::registers();
        Mem::wrapped(Binary::wrapped(
            BinOp::Plus,
            Temp::wrapped(r.stack.into()),
            Const::<i64>::int(offset),
        ))
    }

    fn allocate(&mut self, ident: &str, ptr: bool) -> Expr {
        self.offset -= i64::try_from(Self::word_size()).unwrap();
        self.variables
            .insert(ident.to_string(), Variable::new(self.offset, ptr));
        self.get(ident)
    }

    fn map(&self) -> HashMap<Location, bool> {
        self.variables
            .values()
            .map(|variable| (Location::Frame(variable.offset), variable.ptr))
            .collect()
    }

    fn prologue(&self) -> Vec<isa::A64> {
        let r = Self::registers();
        let mut prologue = vec![];
        // let saves = list(&Opcode::Push);
        // prologue.extend(saves);
        prologue.push(isa::A64::Sub(
            r.stack.into(),
            r.stack.into(),
            format!(
                "#{}",
                next_multiple_of(
                    self.offset.abs() + i64::try_from(Self::word_size()).unwrap(),
                    16
                )
            ),
        ));
        prologue.push(isa::A64::StorePair(
            String::from("x29"),
            String::from("x30"),
        ));
        prologue.push(isa::A64::Add(
            String::from("x29"),
            r.stack.into(),
            String::from("#16"),
        ));
        for (i, formal) in self.formals.iter().enumerate() {
            prologue.push(isa::A64::StoreImmediate(
                formal.register.into(),
                String::from("x29"),
                i64::try_from((i + 1) * Self::word_size()).unwrap(),
            ));
        }
        prologue
    }

    fn epilogue(&self) -> Vec<isa::A64> {
        vec![
            isa::A64::LoadPair(String::from("x29"), String::from("x30")),
            isa::A64::Add(
                String::from("sp"),
                String::from("sp"),
                format!(
                    "#{}",
                    next_multiple_of(
                        self.offset.abs() + i64::try_from(Self::word_size()).unwrap(),
                        16
                    )
                ),
            ),
            isa::A64::Ret,
        ]
    }

    fn header() -> &'static str {
        // _set_stack_base is an internal runtime function that sets the
        // base stack pointer used for garbage collection scanning.
        indoc::indoc! {"
                    .section __TEXT,__text,regular,pure_instructions
                    .global _main
                    .p2align 2
            _main:
                    stp x29, x30, [sp, #-16]!
                    mov x0, sp
                    bl _set_stack_base
                    bl main
                    ldp x29, x30, [sp], #16
                    ret
        "}
    }

    fn registers() -> RegisterMap {
        RegisterMap {
            callee: &[
                "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30",
            ],
            temporary: &["x9", "x10", "x11", "x12", "x13", "x14", "x15"],
            argument: &["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"],
            ret: "x0",
            frame: "x29",
            stack: "sp",
        }
    }

    fn word_size() -> usize {
        8
    }
}

#[derive(Debug)]
struct Variable {
    offset: i64,
    ptr: bool,
}

impl Variable {
    fn new(offset: i64, ptr: bool) -> Variable {
        Self { offset, ptr }
    }
}

#[derive(Debug)]
struct Formal {
    register: &'static str,
}

impl Formal {
    fn new(register: &'static str) -> Self {
        Self { register }
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

use crate::{
    ast::{node::FuncDecl, Type},
    backend::kyir::{
        arch::{Location, RegisterMap, ReturnRegisters},
        ir::{AddressStrategy, BinOp, Binary, Const, Expr, Mem, Temp},
        Frame, Instr, Opcode,
    },
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Amd64 {
    variables: HashMap<String, Variable>,
    formals: Vec<Formal>,
    label: String,
    offset: i64,
}

impl Frame for Amd64 {
    fn new(func: &FuncDecl) -> Self {
        let registers = Self::registers();
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
                .map(|(i, _)| Formal::new(registers.argument[i]))
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
        let registers = Self::registers();
        Mem::wrapped(Binary::wrapped(
            BinOp::Plus,
            Temp::wrapped(registers.frame.into()),
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

    fn prologue(&self) -> Vec<Instr> {
        let registers = Self::registers();
        let mut prologue = vec![];
        prologue.push(Instr::Oper {
            opcode: Opcode::Push,
            src: registers.frame.into(),
            dst: String::new(),
            jump: None,
        }); // pushq %rbp
        let saves = list(Opcode::Push);
        prologue.extend(saves);
        prologue.push(Instr::oper(
            Opcode::Move(AddressStrategy::Immediate),
            registers.frame.into(),
            registers.stack.into(),
            None,
        )); // movq %rsp, %rbp
        prologue.push(Instr::oper(
            Opcode::Sub,
            registers.stack.into(),
            format!("${}", next_multiple_of(self.offset.abs(), 16)),
            None,
        )); // subq $(), %rsp
        for (i, formal) in self.formals.iter().enumerate() {
            prologue.push(Instr::oper(
                Opcode::Move(AddressStrategy::Immediate),
                format!(
                    "{}(%{})",
                    -i64::try_from((i + 1) * Self::word_size()).unwrap(),
                    registers.frame
                ),
                formal.register.into(),
                None,
            )); // movq -(i + 1)*8(%rbp), %formal.register
        }
        prologue
    }

    fn epilogue(&self) -> Vec<Instr> {
        let registers = Self::registers();
        let mut epilogue = vec![];
        epilogue.push(Instr::Oper {
            opcode: Opcode::Add,
            src: format!("${}", next_multiple_of(self.offset.abs(), 16)),
            dst: registers.stack.into(),
            jump: None,
        }); // addq $(), %rsp
        let restores: Vec<_> = list(Opcode::Pop).into_iter().rev().collect();
        epilogue.extend(restores);
        epilogue.push(Instr::Oper {
            opcode: Opcode::Pop,
            src: registers.frame.to_string(),
            dst: String::new(),
            jump: None,
        }); // popq %rbp
        epilogue.push(Instr::Oper {
            opcode: Opcode::Ret,
            src: String::new(),
            dst: String::new(),
            jump: None,
        }); // retq
        epilogue
    }

    fn header() -> &'static str {
        // _set_stack_base is an internal runtime function that sets the
        // base stack pointer used for garbage collection scanning.
        indoc::indoc! {"
                    .text
                    .global _main
            _main:
                    movq %rbp, %rdi
                    callq _set_stack_base
                    callq main
                    ret
        "}
    }

    fn registers() -> RegisterMap {
        RegisterMap {
            caller: &["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
            callee: &["rbx", "r12", "r13", "r14", "r15"],
            temporary: &["r10", "r11"],
            argument: &["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
            ret: ReturnRegisters {
                address: "rip",
                value: "rax",
            },
            frame: "rbp",
            stack: "rsp",
        }
    }

    fn word_size() -> usize {
        8
    }
}

fn list(opcode: Opcode) -> Vec<Instr> {
    // TODO: only push registers that are actually used.
    // Currently, there's no way to know which registers are used
    // in which function.
    Amd64::registers()
        .callee
        .iter()
        .filter(|&&reg| reg != "rbp" && reg != "rsp")
        .map(move |&reg| Instr::Oper {
            opcode: opcode.clone(),
            src: reg.to_string(),
            dst: String::new(),
            jump: None,
        })
        .collect()
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

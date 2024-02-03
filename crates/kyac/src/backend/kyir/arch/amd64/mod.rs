mod red_zone;

use crate::{
    ast::{node::FuncDecl, Type},
    backend::kyir::{
        arch::{RegisterMap, ReturnRegisters},
        ir::{BinOp, Binary, Const, ESeq, Expr, Mem, Move, Seq, Temp},
        Codegen, Frame, Instr, Opcode,
    },
    pass::SymbolTable,
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Amd64 {
    variables: HashMap<String, i64>,
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
            variables.insert(param.name.to_string(), offset);
            offset -= i64::try_from(Self::word_size()).unwrap();
        }
        Self {
            formals: func
                .params
                .iter()
                .enumerate()
                .map(|(i, param)| Formal::new(registers.argument[i], param.name.to_string()))
                .collect(),
            label: func.name.to_string(),
            variables,
            offset,
        }
    }

    fn label(&self) -> &String {
        &self.label
    }

    fn get_offset(&self, ident: &str) -> i64 {
        self.variables.get(ident).copied().unwrap()
    }

    fn get(&self, ident: &str, temp: Option<String>, index: Option<usize>) -> Expr {
        let offset = self.get_offset(ident);
        let registers = Self::registers();
        if self.formals.iter().any(|formal| ident == formal.ident) {
            if let (Some(temp), Some(index)) = (temp, index) {
                // Special case: record field access on a function argument, so it is not in the
                // current frame (passed to the function using `lea`). So: first move the record
                // ptr into %temp, then dereference it, accessing the (index*8)th word.
                // Finally, return %temp.
                return ESeq::wrapped(
                    Seq::wrapped(
                        // movq offset(%rbp), %temp
                        Move::wrapped(
                            Temp::wrapped(temp.clone()),
                            Mem::wrapped(Binary::wrapped(
                                BinOp::Plus,
                                Temp::wrapped(registers.frame.into()),
                                Const::<i64>::int(offset),
                            )),
                        ),
                        // movq index*8(%temp), %temp
                        Some(Move::wrapped(
                            Temp::wrapped(temp.clone()),
                            Mem::wrapped(Binary::wrapped(
                                BinOp::Plus,
                                Temp::wrapped(temp.clone()),
                                Const::<i64>::int(
                                    i64::try_from(index * Self::word_size()).unwrap(),
                                ),
                            )),
                        )),
                    ),
                    Temp::wrapped(temp),
                );
            }
        }
        let offset = if let Some(index) = index {
            // The address mapped to `ident` is one word before the start of the record in the frame
            // because one word is used to store where the record begins, so index + 1
            offset - i64::try_from((index + 1) * Self::word_size()).unwrap()
        } else {
            offset
        };
        Mem::wrapped(Binary::wrapped(
            BinOp::Plus,
            Temp::wrapped(registers.frame.into()),
            Const::<i64>::int(offset),
        ))
    }

    fn allocate(&mut self, symbols: &SymbolTable, ident: &str, ty: Option<&Type>) -> Expr {
        let rec = self.offset;
        self.offset -= i64::try_from(match ty {
            Some(Type::UserDefined(ty)) => {
                let rec = symbols.get(ty).unwrap().record();
                // The magic +1 is here because the first word is used to
                // store the address of the record in the frame.
                (rec.fields.len() + 1) * Self::word_size()
            }
            _ => 8,
        })
        .unwrap();
        self.variables.insert(
            ident.to_string(),
            if matches!(ty, Some(Type::UserDefined(_))) {
                rec
            } else {
                self.offset
            },
        );
        self.get(ident, None, None)
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
        prologue.push(Instr::oper(
            Opcode::Move,
            registers.frame.into(),
            registers.stack.into(),
            None,
        )); // movq %rsp, %rbp
        for (i, formal) in self.formals.iter().enumerate() {
            prologue.push(Instr::oper(
                Opcode::Move,
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
        vec![
            Instr::Oper {
                opcode: Opcode::Pop,
                src: registers.frame.to_string(),
                dst: String::new(),
                jump: None,
            }, // popq %rbp
            Instr::Oper {
                opcode: Opcode::Ret,
                src: String::new(),
                dst: String::new(),
                jump: None,
            }, // retq
        ]
    }

    fn passes(codegen: &mut Codegen<Self>) {
        red_zone::run(codegen);
    }

    fn header() -> String {
        String::from(".text\n.global main\n")
    }

    fn registers() -> RegisterMap {
        RegisterMap {
            caller: &["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
            callee: &["rbx", "r12", "r13", "r14", "r15", "rsp", "rbp"],
            temporary: &["rax", "r10", "r11"],
            argument: &["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
            ret: ReturnRegisters {
                address: "rip",
                value: "rax",
            },
            frame: "rbp",
            stack: "rsp",
        }
    }

    fn offset(&self) -> i64 {
        self.offset
    }

    fn word_size() -> usize {
        8
    }
}

#[derive(Debug)]
struct Formal {
    register: &'static str,
    ident: String,
}

impl Formal {
    fn new(register: &'static str, ident: String) -> Self {
        Self { register, ident }
    }
}

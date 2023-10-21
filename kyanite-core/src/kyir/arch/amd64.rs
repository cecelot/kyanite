use std::collections::HashMap;

use crate::{
    ast::node::FuncDecl,
    kyir::{BinOp, Expr},
};

use super::{Frame, RegisterMap, ReturnRegisters};

pub struct Amd64 {
    /// Map of local variable names to their offsets from the frame pointer
    locals: HashMap<String, i64>,
    formals: Vec<&'static str>,
}

impl Frame for Amd64 {
    fn new(func: &FuncDecl) -> Self {
        let registers: RegisterMap = Self::registers();
        assert!(func.params.len() <= 6);
        Self {
            locals: HashMap::new(),
            formals: func
                .params
                .iter()
                .enumerate()
                .map(|(i, _)| registers.argument[i])
                .collect(),
        }
    }

    fn get(&self, ident: &str) -> Box<Expr> {
        let offset = self.locals.get(ident).copied().unwrap();
        let registers = Self::registers();
        Box::new(Expr::Mem(Box::new(Expr::Binary(
            BinOp::Plus,
            Box::new(Expr::Temp(registers.frame.to_string())),
            Box::new(Expr::ConstInt(offset)),
        ))))
    }

    fn allocate(&mut self, ident: &str) -> Box<Expr> {
        let offset = self.offset();
        self.locals.insert(ident.to_string(), offset);
        self.get(ident)
    }

    fn prologue(&self) -> String {
        let registers = Self::registers();
        let mut prologue = String::new();
        prologue.push_str(&format!("pushq %{}\n", registers.frame));
        prologue.push_str(&format!(
            "movq %{}, %{}\n",
            registers.stack, registers.frame
        ));
        let offset = self.offset();
        for (i, formal) in self.formals.iter().enumerate() {
            prologue.push_str(&format!(
                "movq %{formal}, {}(%{})\n",
                offset - i64::try_from(i * Self::word_size()).unwrap(),
                registers.frame
            ));
        }
        prologue
    }

    fn epilogue(&self) -> String {
        let registers = Self::registers();
        let mut epilogue = String::with_capacity(3);
        // TODO: set return value (handle by function body?)
        epilogue.push_str(&format!("popq %{}\n", registers.frame));
        epilogue.push_str("ret\n");
        epilogue
    }

    fn registers() -> RegisterMap {
        RegisterMap {
            caller: vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
            callee: vec!["rbx", "r12", "r13", "r14", "r15", "rsp", "rbp"],
            temporary: vec!["rax", "r10", "r11"],
            argument: vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
            ret: ReturnRegisters {
                address: "rip",
                value: "rax",
            },
            frame: "rbp",
            stack: "rsp",
        }
    }

    fn offset(&self) -> i64 {
        let offset: i64 = ((self.locals.len() + 1) * Self::word_size())
            .try_into()
            .unwrap();
        // Offsets are negative from the frame pointer
        -offset
    }

    fn word_size() -> usize {
        8
    }
}

use std::collections::HashMap;

use crate::{
    ast::{node::FuncDecl, Type},
    kyir::{BinOp, Expr, Stmt},
    pass::{Symbol, SymbolTable},
    token::Token,
};

use super::{Frame, RegisterMap, ReturnRegisters};

#[derive(Debug)]
pub struct Amd64 {
    variables: HashMap<String, i64>,
    formals: Vec<(&'static str, String)>,
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
                .map(|(i, param)| (registers.argument[i], param.name.to_string()))
                .collect(),
            variables,
            offset,
        }
    }

    fn get_offset(&self, ident: &str) -> i64 {
        self.variables.get(ident).copied().unwrap()
    }

    fn get(&self, ident: &str, temp: Option<String>, index: Option<usize>) -> Box<Expr> {
        let offset = self.get_offset(ident);
        let registers = Self::registers();
        if self.formals.iter().any(|(_, name)| ident == name) {
            if let (Some(temp), Some(index)) = (temp, index) {
                // Special case: record field access on a function argument, so it is not in the
                // current frame (passed to the function using `lea`). So: first move the record
                // ptr into %temp, then dereference it, accessing the (index*8)th word.
                // Finally, return %temp.
                return Box::new(Expr::eseq(
                    Box::new(Stmt::Seq {
                        // movq offset(%rbp), %temp
                        left: Box::new(Stmt::Move {
                            target: Box::new(Expr::Temp(temp.clone())),
                            expr: Box::new(Expr::Mem(Box::new(Expr::Binary {
                                op: BinOp::Plus,
                                left: Box::new(Expr::Temp(registers.frame.to_string())),
                                right: Box::new(Expr::ConstInt(offset)),
                            }))),
                        }),
                        // movq index*8(%temp), %temp
                        right: Some(Box::new(Stmt::Move {
                            target: Box::new(Expr::Temp(temp.clone())),
                            expr: Box::new(Expr::Mem(Box::new(Expr::Binary {
                                op: BinOp::Plus,
                                left: Box::new(Expr::Temp(temp.clone())),
                                right: Box::new(Expr::ConstInt(
                                    i64::try_from(index * Self::word_size()).unwrap(),
                                )),
                            }))),
                        })),
                    }),
                    Box::new(Expr::Temp(temp)),
                ));
            }
        }
        let offset = if let Some(index) = index {
            // The address mapped to `ident` is one word before the start of the record in the frame
            // because one word is used to store where the record begins, so index + 1
            offset - i64::try_from((index + 1) * Self::word_size()).unwrap()
        } else {
            offset
        };
        Box::new(Expr::Mem(Box::new(Expr::Binary {
            op: BinOp::Plus,
            left: Box::new(Expr::Temp(registers.frame.to_string())),
            right: Box::new(Expr::ConstInt(offset)),
        })))
    }

    fn allocate(&mut self, symbols: &SymbolTable, ident: &str, ty: Option<&Type>) -> Box<Expr> {
        let rec = self.offset;
        self.offset -= i64::try_from(match ty {
            Some(Type::UserDefined(ty)) => match symbols.get(&Token::from(ty.clone())).unwrap() {
                Symbol::Record(rec) => rec.fields.len() * Self::word_size(),
                _ => unreachable!(),
            },
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

    fn prologue(&self) -> String {
        let registers = Self::registers();
        let mut prologue = String::new();
        prologue.push_str(&format!("pushq %{}\n", registers.frame));
        prologue.push_str(&format!(
            "movq %{}, %{}\n",
            registers.stack, registers.frame
        ));
        for (i, (formal, _)) in self.formals.iter().enumerate() {
            prologue.push_str(&format!(
                "movq %{}, {}(%{})\n",
                formal,
                -i64::try_from((i + 1) * Self::word_size()).unwrap(),
                registers.frame
            ));
        }
        prologue
    }

    fn epilogue(&self) -> String {
        let registers = Self::registers();
        let mut epilogue = String::with_capacity(3);
        epilogue.push_str(&format!("popq %{}\n", registers.frame));
        epilogue.push_str("retq\n");
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
        self.offset
    }

    fn word_size() -> usize {
        8
    }
}

mod alloc;
pub mod arch;
mod ir;
mod opcode;
mod translate;

use crate::{
    ast::Decl,
    backend::kyir::{
        alloc::Registers,
        arch::{ArchInstr, Frame},
        ir::{
            AddressStrategy, Binary, CJump, Call, Const, Expr, Jump, Label, Mem, Move, RelOp, Seq,
            Stmt, Temp,
        },
        opcode::Opcode,
        translate::Translator,
    },
    pass::{AccessMap, SymbolTable},
};
use std::{
    collections::HashMap,
    fmt::{Display, Write},
    sync::atomic::{AtomicUsize, Ordering},
};

pub fn asm<F: Frame>(ast: &[Decl], symbols: &SymbolTable, accesses: &AccessMap) -> String {
    let mut translator: Translator<F> = Translator::new(accesses, symbols);
    let naive = translator.translate(ast);
    let ir = translate::canonicalize(naive);
    let mut codegen: Codegen<F> = Codegen::new(translator.functions(), translator.strings(), ast);
    let instrs = codegen.assembly(ir);
    let registers = alloc::registers::<F, Instr>(instrs);
    codegen.format(&registers)
}

#[derive(Debug)]
pub struct Codegen<'a, F: Frame> {
    asm: Vec<AsmInstr<Instr>>,
    functions: &'a HashMap<usize, F>,
    strings: &'a HashMap<String, String>,
    idents: HashMap<String, usize>,
}

impl<'a, F: Frame> Codegen<'a, F> {
    fn new(
        functions: &'a HashMap<usize, F>,
        strings: &'a HashMap<String, String>,
        ast: &[Decl],
    ) -> Self {
        Self {
            idents: ast
                .iter()
                .filter_map(|decl| {
                    if let Decl::Function(decl) = decl {
                        Some((decl.name.to_string(), decl.id))
                    } else {
                        None
                    }
                })
                .collect(),
            asm: Vec::new(),
            functions,
            strings,
        }
    }

    #[must_use]
    fn assembly(&mut self, ir: Vec<Stmt>) -> &Vec<AsmInstr<Instr>> {
        ir.into_iter().for_each(|stmt| stmt.assembly(self));
        self.epilogues();
        self.strings();
        &self.asm
    }

    fn strings(&mut self) {
        let instrs = self.strings.iter().flat_map(|(addr, s)| {
            vec![
                AsmInstr::new(Instr::oper(
                    Opcode::Label(addr.clone()),
                    String::new(),
                    String::new(),
                    None,
                )),
                AsmInstr::new(Instr::oper(
                    Opcode::Data {
                        kind: "asciz".into(),
                        value: format!("\"{s}\""),
                    },
                    String::new(),
                    String::new(),
                    None,
                )),
            ]
        });
        self.asm.extend(instrs);
    }

    fn epilogues(&mut self) {
        let mut instrs = vec![];
        for (name, id) in &self.idents {
            let function = self.functions.get(id).unwrap();
            instrs.push(AsmInstr::new(Instr::Oper {
                opcode: Opcode::Label(format!("{name}.epilogue")),
                src: String::new(),
                dst: String::new(),
                jump: None,
            }));
            for instr in function.epilogue() {
                instrs.push(AsmInstr::new(instr));
            }
        }
        self.asm.extend(instrs);
    }

    fn access(mem: &Expr) -> (String, i64) {
        let bin = match mem {
            Expr::Mem(mem) => mem.expr.binary().unwrap(),
            Expr::Binary(bin) => bin,
            _ => panic!("Expected `Expr::Mem` or `Expr::Binary`"),
        };
        (String::from("x29"), bin.right.int().unwrap().abs())
    }

    fn format(self, registers: &Registers) -> String {
        let mut asm = String::new();
        for mut instr in self.asm {
            match &mut instr.inner {
                Instr::Oper {
                    opcode: Opcode::LoadEffective((tmp, _)),
                    ..
                } => {
                    *tmp = registers.get::<F>(tmp);
                }
                Instr::Oper {
                    opcode: Opcode::AddTriple((dst, src, _)),
                    ..
                }
                | Instr::Oper { dst, src, .. } => {
                    *dst = registers.get::<F>(dst);
                    *src = registers.get::<F>(src);
                }
                Instr::Call { .. } => {}
            }

            writeln!(asm, "{instr}").unwrap();
        }
        asm
    }

    fn emit(&mut self, instr: Instr) {
        self.asm.push(AsmInstr::new(instr));
    }
}

trait Assembly<R> {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) -> R;
}

impl Assembly<()> for Stmt {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) {
        match self {
            Self::Jump(jmp) => jmp.assembly(codegen),
            Self::Label(label) => label.assembly(codegen),
            Self::Move(m) => m.assembly(codegen),
            Self::CJump(cjmp) => cjmp.assembly(codegen),
            Self::Seq(seq) => seq.assembly(codegen),
            Self::Expr(e) => {
                e.assembly(codegen);
            }
            Self::Noop => {}
        }
    }
}

impl Assembly<String> for Expr {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) -> String {
        match self {
            Self::Binary(bin) => bin.assembly(codegen),
            Self::ConstInt(i) => i.assembly(codegen),
            Self::Mem(mem) => mem.assembly(codegen),
            Self::Call(call) => call.assembly(codegen),
            Self::Temp(t) => t.name.clone(),
            Self::Dereferenced(t) => format!("[{}]", t.name),
            Self::ConstStr(name) => {
                let tmp = Temp::next();
                codegen.emit(Instr::oper(
                    Opcode::LoadEffective((tmp.clone(), name.clone())),
                    String::new(),
                    String::new(),
                    None,
                ));
                tmp
            }
            Self::ConstFloat(_) => todo!(),
            Self::ESeq { .. } => panic!("`Expr::ESeq` not removed by canonicalization"),
        }
    }
}

impl Assembly<String> for Const<i64> {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) -> String {
        let name = Temp::next();
        codegen.emit(Instr::Oper {
            opcode: Opcode::Move(AddressStrategy::Immediate),
            dst: name.clone(),
            src: format!("#{}", self.value),
            jump: None,
        });
        name
    }
}

impl Assembly<String> for Binary {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) -> String {
        let right = self.right.assembly(codegen);
        let left = self.left.assembly(codegen);
        codegen.emit(Instr::Oper {
            opcode: Opcode::from(self.op),
            dst: left.clone(),
            src: right,
            jump: None,
        });
        left
    }
}

impl Assembly<String> for Mem {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) -> String {
        let dst = Temp::next();
        let (rbp, offset) = Codegen::<F>::access(&self.expr);
        let src = format!("[{rbp}, #{offset}]");
        let oper = Instr::Oper {
            opcode: Opcode::LoadImmediate,
            dst: dst.clone(),
            src,
            jump: None,
        };
        codegen.emit(oper);
        dst
    }
}

impl Assembly<String> for Call {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) -> String {
        let args: Vec<_> = self
            .args
            .iter()
            .map(|arg| arg.assembly(codegen))
            .enumerate()
            .map(|(i, arg)| Instr::Oper {
                opcode: Opcode::Move(AddressStrategy::Immediate),
                dst: F::registers().argument[i].to_owned(),
                src: arg,
                jump: None,
            })
            .collect();
        args.into_iter().for_each(|arg| codegen.emit(arg));
        codegen.emit(Instr::Call {
            name: if BUILTINS.contains(&self.name.as_ref()) {
                F::prefixed(&self.name)
            } else {
                self.name.clone()
            },
        });
        F::registers().ret.value.to_owned()
    }
}

impl Assembly<()> for Jump {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) {
        codegen.emit(Instr::Oper {
            opcode: Opcode::Jump,
            dst: String::new(),
            src: String::new(),
            jump: Some(self.target.clone()),
        });
    }
}

impl Assembly<()> for Label {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) {
        let id = codegen.idents.get(&self.name).copied();
        codegen.emit(Instr::Oper {
            opcode: Opcode::Label(self.name.clone()),
            dst: String::new(),
            src: String::new(),
            jump: None,
        });
        if let Some(id) = id {
            let function = codegen.functions.get(&id).unwrap();
            for instr in function.prologue() {
                codegen.emit(instr);
            }
        }
    }
}

impl Assembly<()> for Move {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) {
        if let Expr::Binary(ref bin) = *self.expr {
            if bin.left.temp().is_some_and(|t| t == "sp") {
                let (rbp, offset) = Codegen::<F>::access(&self.expr);
                codegen.emit(Instr::Oper {
                    opcode: Opcode::LoadImmediate,
                    dst: self.target.temp().unwrap(),
                    src: format!("[{rbp}, #{offset}]"),
                    jump: None,
                });
                return;
            }
        }
        let mut src = match *self.expr {
            Expr::Mem(_) => {
                let temp = Temp::next();
                let (rbp, offset) = Codegen::<F>::access(&self.expr);
                codegen.emit(Instr::Oper {
                    opcode: Opcode::LoadImmediate,
                    dst: temp.clone(),
                    src: format!("[{rbp}, #{offset}]"),
                    jump: None,
                });
                temp
            }
            _ => self.expr.assembly(codegen),
        };
        let mut dst = match *self.target {
            Expr::Mem(_) | Expr::Binary { .. } => {
                let (rbp, offset) = Codegen::<F>::access(&self.target);
                format!("[{rbp}, #{offset}]")
            }
            _ => self.target.temp().unwrap(),
        };
        if dst.starts_with('[') {
            std::mem::swap(&mut dst, &mut src);
        }
        codegen.emit(Instr::Oper {
            opcode: if src.starts_with('[') {
                if matches!(*self.expr, Expr::Dereferenced(_)) {
                    Opcode::LoadImmediate
                } else {
                    Opcode::StoreImmediate
                }
            } else {
                Opcode::Move(self.strategy)
            },
            dst,
            src,
            jump: None,
        });
    }
}

impl Assembly<()> for CJump {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) {
        let tmp = self.condition.assembly(codegen);
        if let Expr::ConstInt(_) = *self.condition {
            let one = Temp::next();
            codegen.emit(Instr::oper(
                Opcode::Move(AddressStrategy::Immediate),
                one.clone(),
                "#1".into(),
                None,
            ));
            codegen.emit(Instr::oper(Opcode::Cmp(RelOp::Equal), tmp, one, None));
            codegen.emit(Instr::oper(
                Opcode::CJump(self.op.into()),
                String::new(),
                String::new(),
                Some(self.t.clone()),
            ));
        } else {
            codegen.emit(Instr::Oper {
                opcode: Opcode::CJump(self.op.into()),
                dst: String::new(),
                src: String::new(),
                jump: Some(self.t.clone()),
            });
        }
    }
}

impl Assembly<()> for Seq {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) {
        self.left.assembly(codegen);
        if let Some(right) = &self.right {
            right.assembly(codegen);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instr {
    Oper {
        opcode: Opcode,
        dst: String,
        src: String,
        jump: Option<String>,
    },
    Call {
        name: String,
    },
}

impl Instr {
    fn oper(opcode: Opcode, dst: String, src: String, jump: Option<String>) -> Self {
        Self::Oper {
            opcode,
            dst,
            src,
            jump,
        }
    }
}

impl ArchInstr for Instr {
    fn defines(&self) -> Vec<String> {
        match &self {
            Instr::Oper {
                dst,
                opcode: Opcode::Move(_) | Opcode::LoadImmediate,
                ..
            }
            | Instr::Oper {
                opcode: Opcode::LoadEffective((dst, _)),
                ..
            } => vec![dst.clone()],
            _ => vec![],
        }
    }

    fn uses(&self) -> Vec<String> {
        match &self {
            Instr::Oper {
                opcode:
                    Opcode::Move(_)
                    | Opcode::Cmp(_)
                    | Opcode::Add
                    | Opcode::Sub
                    | Opcode::Mul
                    | Opcode::Div
                    | Opcode::LoadImmediate
                    | Opcode::StoreImmediate,
                src,
                dst,
                ..
            } => vec![src.clone(), dst.clone()],
            Instr::Oper {
                opcode: Opcode::AddTriple((src, _, _)),
                ..
            }
            | Instr::Oper { src, .. } => vec![src.clone()],
            Instr::Call { .. } => vec![],
        }
    }

    fn to(&self) -> Option<String> {
        match &self {
            Instr::Oper { jump, .. } => jump.clone(),
            Instr::Call { .. } => None,
        }
    }

    fn jump(&self) -> bool {
        matches!(
            self,
            Instr::Oper {
                opcode: Opcode::Jump,
                ..
            }
        )
    }

    fn label(&self) -> Option<String> {
        match &self {
            Instr::Oper {
                opcode: Opcode::Label(label),
                ..
            } => Some(label.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AsmInstr<I> {
    inner: I,
    id: usize,
}

impl AsmInstr<Instr> {
    fn new(instr: Instr) -> Self {
        static ID: AtomicUsize = AtomicUsize::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        Self { inner: instr, id }
    }

    fn operands(&self) -> usize {
        match &self.inner {
            Instr::Oper { opcode, .. } => match opcode {
                Opcode::Jump | Opcode::CJump(_) | Opcode::Push | Opcode::Ret | Opcode::Pop => 1,
                Opcode::Label(_)
                | Opcode::Data { .. }
                | Opcode::StorePair(_)
                | Opcode::LoadPair(_)
                | Opcode::AddTriple(_)
                | Opcode::LoadEffective(_) => 0,
                _ => 2,
            },
            Instr::Call { .. } => 0,
        }
    }
}

impl Display for AsmInstr<Instr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pad = " ".repeat(8);
        match &self.inner {
            Instr::Oper {
                opcode,
                dst,
                src,
                jump,
            } => {
                let pad = match opcode {
                    Opcode::Label(_) => String::new(),
                    _ => pad,
                };
                match opcode {
                    Opcode::Jump => write!(f, "{pad}{opcode} {}", jump.as_ref().unwrap())?,
                    Opcode::CJump(_) => write!(f, "{pad}{opcode} {}", jump.as_ref().unwrap())?,
                    Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                        write!(f, "{pad}{opcode} {dst}, {dst}, {src}")?;
                    }
                    _ => write!(
                        f,
                        "{pad}{opcode} {dst}{} {src}",
                        if self.operands() == 2 { "," } else { "" }
                    )?,
                }
                Ok(())
            }
            Instr::Call { name } => {
                write!(f, "{pad}bl {name}")?;
                Ok(())
            }
        }
    }
}

const BUILTINS: &[&str] = &[
    "max_int",
    "min_int",
    "max_float",
    "min_float",
    "println_bool",
    "println_int",
    "println_float",
    "println_str",
    // internal
    "alloc",
];

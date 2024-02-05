mod alloc;
pub mod arch;
mod ir;
mod opcode;
mod translate;

use crate::{
    ast::Decl,
    backend::kyir::{
        alloc::Registers,
        arch::Frame,
        ir::{Binary, CJump, Call, Const, Expr, Jump, Label, Mem, Move, RelOp, Seq, Stmt, Temp},
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
    let mut codegen: Codegen<F> = Codegen::new(translator.functions(), ast);
    let instrs = codegen.assembly(ir);
    let registers = alloc::registers::<F>(instrs);
    codegen.format(&registers)
}

#[derive(Debug)]
pub struct Codegen<F: Frame> {
    asm: Vec<AsmInstr>,
    functions: HashMap<usize, F>,
    idents: HashMap<String, usize>,
}

impl<F: Frame> Codegen<F> {
    fn new(functions: HashMap<usize, F>, ast: &[Decl]) -> Self {
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
        }
    }

    #[must_use]
    fn assembly(&mut self, ir: Vec<Stmt>) -> &Vec<AsmInstr> {
        ir.into_iter().for_each(|stmt| stmt.assembly(self));
        self.epilogues();
        &self.asm
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
        self.asm.append(&mut instrs);
    }

    fn access(mem: &Expr) -> (String, i64) {
        let bin = match mem {
            Expr::Mem(mem) => mem.expr.binary().unwrap(),
            Expr::Binary(bin) => bin,
            _ => panic!("Expected `Expr::Mem` or `Expr::Binary`"),
        };
        (bin.left.temp().unwrap(), bin.right.int().unwrap())
    }

    fn format(self, registers: &Registers) -> String {
        let mut asm = String::new();
        for mut instr in self.asm {
            if let Instr::Oper { dst, src, .. } = &mut instr.inner {
                *dst = registers.get::<F>(dst);
                *src = registers.get::<F>(src);
            };
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
            Self::Dereferenced(t) => format!("({})", t.name),
            Self::ConstFloat(_) => todo!(),
            Self::ESeq { .. } => panic!("`Expr::ESeq` not removed by canonicalization"),
        }
    }
}

impl Assembly<String> for Const<i64> {
    fn assembly<F: Frame>(&self, codegen: &mut Codegen<F>) -> String {
        let name = Temp::next();
        codegen.emit(Instr::Oper {
            opcode: Opcode::Move,
            dst: name.clone(),
            src: format!("${}", self.value),
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
        let src = format!("{offset}(%{rbp})");
        let oper = Instr::Oper {
            opcode: Opcode::Move,
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
                opcode: Opcode::Move,
                dst: F::registers().argument[i].into(),
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
        format!("%{}", F::registers().ret.value)
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
            if bin.left.temp().is_some_and(|t| t == "rbp") {
                let (rbp, offset) = Codegen::<F>::access(&self.expr);
                codegen.emit(Instr::Oper {
                    opcode: Opcode::Move,
                    dst: self.target.temp().unwrap(),
                    src: format!("{offset}(%{rbp})"),
                    jump: None,
                });
                return;
            }
        }
        let src = match *self.expr {
            Expr::Mem(_) => {
                let temp = Temp::next();
                let (rbp, offset) = Codegen::<F>::access(&self.expr);
                codegen.emit(Instr::Oper {
                    opcode: Opcode::Move,
                    dst: temp.clone(),
                    src: format!("{offset}(%{rbp})"),
                    jump: None,
                });
                temp
            }
            _ => self.expr.assembly(codegen),
        };
        let dst = match *self.target {
            Expr::Mem(_) | Expr::Binary { .. } => {
                let (rbp, offset) = Codegen::<F>::access(&self.target);
                format!("{offset}(%{rbp})")
            }
            _ => self.target.temp().unwrap(),
        };
        codegen.emit(Instr::Oper {
            opcode: Opcode::Move,
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
            codegen.emit(Instr::oper(Opcode::Move, one.clone(), "$1".into(), None));
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AsmInstr {
    inner: Instr,
    id: usize,
}

impl AsmInstr {
    fn new(instr: Instr) -> Self {
        static ID: AtomicUsize = AtomicUsize::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        Self { inner: instr, id }
    }

    fn operands(&self) -> usize {
        match &self.inner {
            Instr::Oper { opcode, .. } => match opcode {
                Opcode::Jump | Opcode::CJump(_) | Opcode::Push | Opcode::Ret | Opcode::Pop => 1,
                Opcode::Label(_) => 0,
                _ => 2,
            },
            Instr::Call { .. } => 0,
        }
    }

    fn to(&self) -> Option<String> {
        match &self.inner {
            Instr::Oper { jump, .. } => jump.as_ref().cloned(),
            Instr::Call { .. } => None,
        }
    }

    fn jump(&self) -> bool {
        matches!(
            self.inner,
            Instr::Oper {
                opcode: Opcode::Jump,
                ..
            }
        )
    }

    fn label(&self) -> Option<String> {
        match &self.inner {
            Instr::Oper {
                opcode: Opcode::Label(label),
                ..
            } => Some(label.clone()),
            _ => None,
        }
    }
}

impl Display for AsmInstr {
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
                    _ => write!(
                        f,
                        "{pad}{opcode} {src}{} {dst}",
                        if self.operands() == 2 { "," } else { "" }
                    )?,
                }
                Ok(())
            }
            Instr::Call { name } => {
                write!(f, "{pad}callq {name}")?;
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
    "alloc",
];

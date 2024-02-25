mod alloc;
pub mod arch;
mod ir;
mod translate;

use crate::{
    ast::Decl,
    backend::kyir::{
        alloc::Registers,
        arch::{ArchInstr, Frame},
        ir::{
            BinOp, Binary, CJump, Call, Const, Expr, Jump, Label, Mem, Move, RelOp, Seq, Stmt, Temp,
        },
        translate::Translator,
    },
    pass::{AccessMap, SymbolTable},
};
use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

pub fn asm<I: ArchInstr, F: Frame<I>>(
    ast: &[Decl],
    symbols: &SymbolTable,
    accesses: &AccessMap,
) -> String {
    let mut translator: Translator<I, F> = Translator::new(accesses, symbols);
    let naive = translator.translate(ast);
    let ir = translate::canonicalize(naive);
    let mut codegen: Codegen<I, F> =
        Codegen::new(translator.functions(), translator.strings(), ast);
    let instrs = codegen.assembly(ir);
    let registers = alloc::registers::<I, F>(instrs);
    codegen.format(&registers)
}

#[derive(Debug)]
pub struct Codegen<'a, I: ArchInstr, F: Frame<I>> {
    asm: Vec<AsmInstr<I>>,
    functions: &'a HashMap<usize, F>,
    strings: &'a HashMap<String, String>,
    idents: HashMap<String, usize>,
}

impl<'a, I: ArchInstr, F: Frame<I>> Codegen<'a, I, F> {
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
    fn assembly(&mut self, ir: Vec<Stmt>) -> &Vec<AsmInstr<I>> {
        ir.into_iter().for_each(|stmt| stmt.assembly(self));
        self.epilogues();
        self.strings();
        &self.asm
    }

    fn strings(&mut self) {
        let instrs = self
            .strings
            .iter()
            .flat_map(|(addr, s)| {
                vec![
                    I::create_label(addr.clone()),
                    I::data_fragment(String::from("asciz"), format!("\"{s}\"")),
                ]
            })
            .map(AsmInstr::new);
        self.asm.extend(instrs);
    }

    fn epilogues(&mut self) {
        let mut instrs = vec![];
        for (name, id) in &self.idents {
            let function = self.functions.get(id).unwrap();
            instrs.push(I::create_label(format!("{name}.epilogue")));
            instrs.extend(function.epilogue());
        }
        self.asm.extend(instrs.into_iter().map(AsmInstr::new));
    }

    fn access(mem: &Expr) -> Option<i64> {
        let bin = match mem {
            Expr::Mem(mem) => Some(mem.expr.binary().unwrap()),
            Expr::Binary(bin)
                if matches!(*bin.right, Expr::ConstInt(_))
                    && bin
                        .left
                        .temp()
                        .is_some_and(|temp| temp == F::registers().stack) =>
            {
                Some(bin)
            }
            _ => None,
        };
        bin.map(|bin| bin.right.int().unwrap().abs())
    }

    fn format(self, registers: &Registers) -> String {
        self.asm.into_iter().fold(String::new(), |s, instr| {
            s + &format!("{}\n", instr.inner.format::<I, F>(registers))
        })
    }

    fn emit(&mut self, instr: I) {
        self.asm.push(AsmInstr::new(instr));
    }
}

trait Assembly<R> {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) -> R;
}

impl Assembly<()> for Stmt {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) {
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
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) -> String {
        match self {
            Self::Binary(bin) => bin.assembly(codegen),
            Self::ConstInt(i) => i.assembly(codegen),
            Self::Mem(mem) => mem.assembly(codegen),
            Self::Call(call) => call.assembly(codegen),
            Self::Temp(t) => t.name.clone(),
            Self::Dereferenced(t) => format!("[{}]", t.name),
            Self::ConstStr(name) => {
                let tmp = Temp::next();
                codegen.emit(I::load_fragment(tmp.clone(), name.clone()));
                tmp
            }
            Self::ConstFloat(_) => todo!(),
            Self::ESeq { .. } => panic!("`Expr::ESeq` not removed by canonicalization"),
        }
    }
}

impl Assembly<String> for Const<i64> {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) -> String {
        let name = Temp::next();
        codegen.emit(I::copy(name.clone(), format!("#{}", self.value)));
        name
    }
}

impl Assembly<String> for Binary {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) -> String {
        let right = self.right.assembly(codegen);
        let left = self.left.assembly(codegen);
        let instr = match self.op {
            BinOp::Plus => I::add(left.clone(), right.clone()),
            BinOp::Minus => I::sub(left.clone(), right.clone()),
            BinOp::Mul => I::mul(left.clone(), right.clone()),
            BinOp::Div => I::div(left.clone(), right.clone()),
            BinOp::Cmp(_) => I::compare(left.clone(), right.clone()),
            BinOp::Xor => todo!(),
        };
        codegen.emit(instr);
        left
    }
}

impl Assembly<String> for Mem {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) -> String {
        let dst = Temp::next();
        let offset = Codegen::<I, F>::access(&self.expr).unwrap();
        codegen.emit(I::load_from_frame(dst.clone(), offset));
        dst
    }
}

impl Assembly<String> for Call {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) -> String {
        let r = F::registers();
        let args: Vec<_> = self
            .args
            .iter()
            .map(|arg| arg.assembly(codegen))
            .enumerate()
            .map(|(i, arg)| I::copy(r.argument[i].to_owned(), arg))
            .collect();
        args.into_iter().for_each(|arg| codegen.emit(arg));
        let instr = if BUILTINS.contains(&self.name.as_ref()) {
            I::call(F::prefixed(&self.name))
        } else {
            I::call(self.name.clone())
        };
        codegen.emit(instr);
        r.ret.value.to_owned()
    }
}

impl Assembly<()> for Jump {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) {
        codegen.emit(I::create_jump(self.target.clone()));
    }
}

impl Assembly<()> for Label {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) {
        let id = codegen.idents.get(&self.name).copied();
        codegen.emit(I::create_label(self.name.clone()));
        if let Some(id) = id {
            let function = codegen.functions.get(&id).unwrap();
            for instr in function.prologue() {
                codegen.emit(instr);
            }
        }
    }
}

impl Assembly<()> for Move {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) {
        let store = matches!(
            *self.target,
            Expr::Mem(_) | Expr::Binary(_) | Expr::Dereferenced(_)
        );
        if store {
            let instr = if let Expr::Dereferenced(ref addr) = *self.target {
                let src = self.expr.assembly(codegen);
                I::store_to_address(src, addr.name.to_string())
            } else if let Some(offset) = Codegen::<I, F>::access(&self.target) {
                let src = self.expr.assembly(codegen);
                I::store_to_frame(src, offset)
            } else {
                unimplemented!()
            };
            codegen.emit(instr);
        } else {
            let instr = if let Expr::Dereferenced(ref addr) = *self.expr {
                let dst = self.target.assembly(codegen);
                I::load_from_address(dst, addr.name.to_string())
            } else if let Some(offset) = Codegen::<I, F>::access(&self.expr) {
                let dst = self.target.assembly(codegen);
                I::load_from_frame(dst, offset)
            } else if let Expr::ConstInt(ref i) = *self.expr {
                I::copy_int(self.target.assembly(codegen), i.value)
            } else {
                I::copy(self.target.assembly(codegen), self.expr.assembly(codegen))
            };
            codegen.emit(instr);
        }
    }
}

impl Assembly<()> for CJump {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) {
        let tmp = self.condition.assembly(codegen);
        if let Expr::ConstInt(_) = *self.condition {
            let one = Temp::next();
            codegen.emit(I::copy_int(one.clone(), 1));
            codegen.emit(I::compare(tmp, one));
        }
        codegen.emit(I::conditional_jump(self.t.clone(), self.op.into()));
    }
}

impl Assembly<()> for Seq {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) {
        self.left.assembly(codegen);
        if let Some(right) = &self.right {
            right.assembly(codegen);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AsmInstr<I: ArchInstr> {
    inner: I,
    id: usize,
}

impl<I: ArchInstr> AsmInstr<I> {
    fn new(inner: I) -> Self {
        static ID: AtomicUsize = AtomicUsize::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        Self { inner, id }
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

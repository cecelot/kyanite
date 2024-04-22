mod alloc;
pub mod arch;
mod ir;
mod translate;

use crate::{
    ast::Decl,
    backend::kyir::{
        alloc::Registers,
        arch::{ArchInstr, FlowGraphMeta, Frame},
        ir::{
            BinOp, Binary, CJump, Call, Const, Expr, Jump, Label, Mem, Move, RelOp, Seq, Stmt, Temp,
        },
        translate::Translator,
    },
    pass::{ResolvedMetaInfo, SymbolTable},
};
use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

pub fn asm<I: ArchInstr, F: Frame<I>>(
    ast: &[Decl],
    symbols: &SymbolTable,
    meta: &ResolvedMetaInfo,
    opt: bool,
) -> String {
    let mut translator: Translator<I, F> = Translator::new(symbols, meta);
    let naive = translator.translate(ast);
    let mut codegen: Codegen<I, F> =
        Codegen::new(translator.functions(), translator.constants(), ast);
    let instrs = if opt {
        let quadruples: Vec<_> = translate::canonicalize(ir::opt::optimize(naive))
            .into_iter()
            .flat_map(ir::opt::quadruple::Flatten::flatten)
            .collect();
        quadruples.iter().for_each(ir::opt::quadruple::verify);
        codegen.assembly(quadruples)
    } else {
        codegen.assembly(translate::canonicalize(naive))
    };
    for instr in instrs {
        log::trace!(
            "[{}] {} (defines: {:?}, uses: {:?})",
            instr.id,
            instr.inner,
            instr.defines(),
            instr.uses()
        );
    }
    let registers = alloc::registers::<I, F>(instrs);
    codegen.format(&registers)
}

#[derive(Debug)]
pub struct Codegen<'a, I: ArchInstr, F: Frame<I>> {
    asm: Vec<AsmInstr<I>>,
    functions: &'a HashMap<usize, F>,
    constants: &'a HashMap<String, Vec<String>>,
    idents: HashMap<String, usize>,
}

impl<'a, I: ArchInstr, F: Frame<I>> Codegen<'a, I, F> {
    fn new(
        functions: &'a HashMap<usize, F>,
        constants: &'a HashMap<String, Vec<String>>,
        ast: &[Decl],
    ) -> Self {
        Self {
            idents: ast
                .iter()
                .filter_map(|decl| {
                    if let Decl::Function(decl) = decl {
                        Some(vec![(decl.name.to_string(), decl.id)])
                    } else if let Decl::Class(class) = decl {
                        Some(
                            class
                                .methods
                                .iter()
                                .map(|m| (format!("{}.{}", class.name, m.name), m.id))
                                .collect(),
                        )
                    } else {
                        None
                    }
                })
                .flatten()
                .collect(),
            asm: Vec::new(),
            functions,
            constants,
        }
    }

    #[must_use]
    fn assembly(&mut self, ir: Vec<Stmt>) -> &Vec<AsmInstr<I>> {
        ir.into_iter().for_each(|stmt| stmt.assembly(self));
        self.epilogues();
        self.constants();
        &self.asm
    }

    fn constants(&mut self) {
        let instrs = self
            .constants
            .iter()
            .flat_map(|(addr, s)| {
                vec![
                    I::proc(addr.clone()),
                    I::data_fragment(
                        String::from("asciz"),
                        s.iter().map(|s| format!("\"{s}\"")).collect(),
                    ),
                ]
            })
            .map(AsmInstr::new);
        self.asm.extend(instrs);
    }

    fn epilogues(&mut self) {
        let mut instrs = vec![];
        for (name, id) in &self.idents {
            let function = self.functions.get(id).unwrap();
            instrs.push(I::proc(format!("{name}.epilogue")));
            instrs.extend(function.epilogue());
        }
        self.asm.extend(instrs.into_iter().map(AsmInstr::new));
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
            Self::ConstStr(name) => {
                let tmp = Temp::next();
                codegen.emit(I::load_fragment(tmp.clone(), name.clone()));
                tmp
            }
            Self::ConstLabel(label) => {
                let tmp = Temp::next();
                codegen.emit(I::label_address(tmp.clone(), label.clone()));
                tmp
            }
            Self::ConstFloat(_) => todo!(),
            Self::ESeq(eseq) => panic!(
                "`Expr::ESeq` not removed by canonicalization (id: {})",
                eseq.id
            ),
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
        codegen.emit(I::load(
            dst.clone(),
            self.base.temp().unwrap(),
            self.offset.value.abs(),
        ));
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
        r.ret.to_owned()
    }
}

impl Assembly<()> for Jump {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) {
        codegen.emit(I::branch(self.target.clone()));
    }
}

impl Assembly<()> for Label {
    fn assembly<I: ArchInstr, F: Frame<I>>(&self, codegen: &mut Codegen<I, F>) {
        let id = codegen.idents.get(&self.name).copied();
        codegen.emit(I::proc(self.name.clone()));
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
        let store = matches!(*self.target, Expr::Mem(_) | Expr::Temp(_));
        if store {
            let instr = if let Expr::Mem(mem) = &*self.target {
                let src = self.expr.assembly(codegen);
                I::store(src, mem.base.temp().unwrap(), mem.offset.value.abs())
            } else {
                I::copy(self.target.assembly(codegen), self.expr.assembly(codegen))
            };
            codegen.emit(instr);
        } else {
            let instr = if let Expr::Mem(mem) = &*self.expr {
                let dst = self.target.assembly(codegen);
                I::load(dst, mem.base.temp().unwrap(), mem.offset.value.abs())
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
        codegen.emit(I::cbranch(self.t.clone(), self.op.into()));
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
    "init_array",
];

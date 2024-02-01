mod alloc;
pub mod arch;
mod opcode;
mod translate;

use crate::{
    ast::Decl,
    backend::kyir::{
        alloc::Registers,
        arch::Frame,
        opcode::Opcode,
        translate::{BinOp, Expr, RelOp, Stmt, Translator},
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
    let instrs = codegen.compile(ir);
    let registers = alloc::registers::<F>(instrs);
    codegen.format(&registers)
}

#[derive(Debug)]
pub struct Codegen<F: Frame> {
    asm: Vec<AsmInstr>,
    functions: HashMap<usize, F>,
    stack: Vec<usize>,
    idents: HashMap<String, usize>,
    call: HashMap<usize, bool>,
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
            call: HashMap::new(),
            stack: vec![],
            functions,
        }
    }

    #[must_use]
    fn compile(&mut self, ir: Vec<Stmt>) -> &Vec<AsmInstr> {
        ir.into_iter().for_each(|stmt| self.compile_stmt(stmt));
        F::passes(self);
        self.emit_epilogues();
        &self.asm
    }

    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Jump(label) => self.compile_jump(label),
            Stmt::Move { target, expr } => self.compile_move(*target, *expr),
            Stmt::Label(label) => self.compile_label(label),
            Stmt::CJump {
                t, op, condition, ..
            } => self.compile_cjump(t, op, &condition),
            Stmt::Expr(e) => {
                self.compile_expr(*e, false);
            }
            Stmt::Seq { left, right } => {
                self.compile_stmt(*left);
                if let Some(right) = right {
                    self.compile_stmt(*right);
                }
            }
            Stmt::Noop => {}
        }
    }

    fn compile_expr(&mut self, expr: Expr, swap: bool) -> String {
        match expr {
            Expr::Binary { op, left, right } => self.compile_binary(op, *left, *right),
            Expr::ConstInt(i) => self.compile_int(i),
            Expr::Mem(mem) => self.compile_mem(*mem, swap),
            Expr::Call { name, args } => self.compile_call(name, args),
            Expr::Temp(t) => t,
            Expr::ConstFloat(_) => todo!(),
            Expr::ESeq { .. } => panic!("`Expr::ESeq` not removed by canonicalization"),
        }
    }

    fn compile_jump(&mut self, label: String) {
        if label.ends_with("epilogue") {
            self.stack.pop();
        }
        self.emit(Instr::Oper {
            opcode: Opcode::Jump,
            dst: String::new(),
            src: String::new(),
            jump: Some(label),
        });
    }

    fn compile_label(&mut self, label: String) {
        let id = self.idents.get(&label).copied();
        self.emit(Instr::Oper {
            opcode: Opcode::Label(label),
            dst: String::new(),
            src: String::new(),
            jump: None,
        });
        if let Some(id) = id {
            self.stack.push(id);
            let function = self.functions.get(&id).unwrap();
            for instr in function.prologue() {
                self.emit(instr);
            }
        }
    }

    fn compile_move(&mut self, target: Expr, expr: Expr) {
        if let Expr::Binary { ref left, .. } = expr {
            if left.temp().is_some_and(|t| t == "rbp") {
                let (rbp, offset) = Self::access(expr);
                self.emit(Instr::Oper {
                    opcode: Opcode::Move,
                    dst: target.temp().unwrap(),
                    src: format!("{offset}(%{rbp})"),
                    jump: None,
                });
                return;
            }
        }
        let src = match expr {
            Expr::Mem(_) => {
                let temp = Temp::next();
                let (rbp, offset) = Self::access(expr);
                self.emit(Instr::Oper {
                    opcode: Opcode::Move,
                    dst: temp.clone(),
                    src: format!("{offset}(%{rbp})"),
                    jump: None,
                });
                temp
            }
            _ => self.compile_expr(expr, false),
        };
        let dst = match target {
            Expr::Mem(_) | Expr::Binary { .. } => {
                let (rbp, offset) = Self::access(target);
                format!("{offset}(%{rbp})")
            }
            _ => target.temp().unwrap(),
        };
        self.emit(Instr::Oper {
            opcode: Opcode::Move,
            dst,
            src,
            jump: None,
        });
    }

    fn compile_cjump(&mut self, t: String, op: BinOp, condition: &Expr) {
        let tmp = self.compile_expr(condition.clone(), false);
        if let Expr::ConstInt(_) = condition {
            let one = Temp::next();
            self.emit(Instr::oper(Opcode::Move, one.clone(), "$1".into(), None));
            self.emit(Instr::oper(Opcode::Cmp(RelOp::Equal), tmp, one, None));
            self.emit(Instr::oper(
                Opcode::CJump(op.into()),
                String::new(),
                String::new(),
                Some(t),
            ));
        } else {
            self.emit(Instr::Oper {
                opcode: Opcode::CJump(op.into()),
                dst: String::new(),
                src: String::new(),
                jump: Some(t),
            });
        }
    }

    fn compile_mem(&mut self, mem: Expr, swap: bool) -> String {
        let name = Temp::next();
        let src = name.clone();
        let (rbp, offset) = Self::access(mem);
        let dst = format!("{offset}(%{rbp})");
        let mut oper = Instr::Oper {
            opcode: Opcode::Move,
            dst,
            src,
            jump: None,
        };
        if swap {
            if let Instr::Oper {
                ref mut dst,
                ref mut src,
                ..
            } = oper
            {
                std::mem::swap(dst, src);
            } else {
                panic!("Expected `Instr::Oper`");
            }
        }
        self.emit(oper);
        name
    }

    fn compile_call(&mut self, name: String, args: Vec<Expr>) -> String {
        if let Some(&id) = self.stack.last() {
            self.call.insert(id, true);
        }
        let args: Vec<_> = args
            .into_iter()
            .map(|arg| self.compile_expr(arg, true))
            .enumerate()
            .map(|(i, arg)| Instr::Oper {
                opcode: Opcode::Move,
                dst: F::registers().argument[i].into(),
                src: arg,
                jump: None,
            })
            .collect();
        args.into_iter().for_each(|arg| self.emit(arg));
        self.emit(Instr::Call { name });
        format!("%{}", F::registers().ret.value)
    }

    fn compile_binary(&mut self, op: BinOp, left: Expr, right: Expr) -> String {
        let right = self.compile_expr(right, false);
        let left = self.compile_expr(left, true);
        self.emit(Instr::Oper {
            opcode: Opcode::from(op),
            dst: left.clone(),
            src: right,
            jump: None,
        });
        left
    }

    fn compile_int(&mut self, i: i64) -> String {
        let name = Temp::next();
        self.emit(Instr::Oper {
            opcode: Opcode::Move,
            dst: name.clone(),
            src: format!("${i}"),
            jump: None,
        });
        name
    }

    fn emit_epilogues(&mut self) {
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

    fn access(mem: Expr) -> (String, i64) {
        let (_, left, right) = match mem {
            Expr::Mem(expr) => expr.binary().unwrap(),
            Expr::Binary { op, left, right } => (op, left, right),
            _ => panic!("Expected `Expr::Mem` or `Expr::Binary`"),
        };
        (left.temp().unwrap(), right.int().unwrap())
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
        match &self.inner {
            Instr::Oper {
                opcode,
                dst,
                src,
                jump,
            } => {
                match opcode {
                    Opcode::Jump => write!(f, "{opcode} {}", jump.as_ref().unwrap())?,
                    Opcode::CJump(_) => write!(f, "{opcode} {}", jump.as_ref().unwrap())?,
                    _ => write!(
                        f,
                        "{opcode} {src}{} {dst}",
                        if self.operands() == 2 { "," } else { "" }
                    )?,
                }
                Ok(())
            }
            Instr::Call { name } => {
                write!(f, "callq {name}")?;
                Ok(())
            }
        }
    }
}

pub struct Temp;

impl Temp {
    fn next() -> String {
        static ID: AtomicUsize = AtomicUsize::new(0);
        format!("T{}", ID.fetch_add(1, Ordering::SeqCst))
    }
}

pub struct Label;

impl Label {
    fn next() -> String {
        static ID: AtomicUsize = AtomicUsize::new(0);
        format!("L{}", ID.fetch_add(1, Ordering::SeqCst))
    }
}

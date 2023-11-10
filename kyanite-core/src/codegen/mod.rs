#![allow(dead_code)]
pub mod llvm;

use std::collections::HashMap;
use std::fmt::Display;

pub use llvm::Ir;
pub use llvm::IrError;

use crate::{
    ast::Decl,
    kyir::{arch::Frame, BinOp, Expr, RelOp, Stmt, Temp},
};

#[derive(Debug)]
pub enum AsmInstr {
    Oper {
        opcode: Opcode,
        dst: String,
        src: String,
        jump: Option<String>,
    },
    Call {
        name: String,
    },
    FrameValue {
        offset: i64,
    },
    Raw(String),
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Cmp(_) => write!(f, "cmp"),
            Opcode::Label(label) => write!(f, ".{}:", label),
            Opcode::Move => write!(f, "movq"),
            Opcode::Jump => write!(f, "jmp"),
            Opcode::CJump(rel) => {
                let cond = match rel {
                    RelOp::Equal => "e",
                    RelOp::NotEqual => "ne",
                    RelOp::Less => "l",
                    RelOp::LessEqual => "le",
                    RelOp::Greater => "g",
                    RelOp::GreaterEqual => "ge",
                };
                write!(f, "j{cond}")
            }
            Opcode::Add => write!(f, "addq"),
            Opcode::Sub => write!(f, "subq"),
            Opcode::Mul => write!(f, "imulq"),
            Opcode::Div => write!(f, "idivq"),
        }
    }
}

impl Display for AsmInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmInstr::Oper {
                opcode,
                dst,
                src,
                jump,
            } => {
                match opcode {
                    Opcode::Jump => write!(f, "{} {}", opcode, jump.as_ref().unwrap())?,
                    Opcode::CJump(_) => write!(f, "{} {}", opcode, jump.as_ref().unwrap())?,
                    _ => write!(f, "{} {} {}", opcode, src, dst)?,
                }

                Ok(())
            }
            AsmInstr::Call { name } => {
                write!(f, "callq {name}")?;
                Ok(())
            }
            AsmInstr::FrameValue { offset } => {
                write!(f, "{}(%rbp)", offset)?;
                Ok(())
            }
            AsmInstr::Raw(raw) => {
                write!(f, "{}", raw)?;
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub enum Opcode {
    Label(String),
    Move,
    Jump,
    Cmp(RelOp),
    CJump(RelOp),
    Add,
    Sub,
    Mul,
    Div,
}

impl From<BinOp> for RelOp {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Cmp(rel) => rel,
            _ => panic!("Cannot convert {:?} to RelOp", value),
        }
    }
}

impl From<BinOp> for Opcode {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Plus => Opcode::Add,
            BinOp::Minus => Opcode::Sub,
            BinOp::Mul => Opcode::Mul,
            BinOp::Div => Opcode::Div,
            BinOp::Xor => todo!(),
            BinOp::Cmp(rel) => Opcode::Cmp(rel),
        }
    }
}

#[derive(Debug)]
struct Codegen<F: Frame> {
    asm: Vec<AsmInstr>,
    functions: HashMap<usize, F>,
    idents: HashMap<String, usize>,
}

impl<F: Frame> Display for Codegen<F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instr in &self.asm {
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}

impl<F: Frame> Codegen<F> {
    pub fn new(ir: Vec<Stmt>, functions: HashMap<usize, F>, ast: &[Decl]) -> Self {
        let mut codegen = Codegen {
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
        };
        codegen.compile(ir);
        codegen
    }

    fn compile(&mut self, ir: Vec<Stmt>) {
        for stmt in ir {
            self.stmt(stmt);
        }
    }

    fn emit(&mut self, instr: AsmInstr) {
        self.asm.push(instr);
    }

    fn stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(e) => {
                self.expr(*e, false);
            }
            Stmt::Jump(label) => {
                self.emit(AsmInstr::Oper {
                    opcode: Opcode::Jump,
                    dst: String::new(),
                    src: String::new(),
                    jump: Some(label),
                });
            }
            Stmt::Noop => {}
            Stmt::Seq { left, right } => {
                self.stmt(*left);
                if let Some(right) = right {
                    self.stmt(*right);
                }
            }
            Stmt::Move { target, expr } => {
                if let Expr::Binary { ref left, .. } = *expr {
                    if left.temp_and("rbp") {
                        let (rbp, offset) = Self::access(*expr);
                        self.emit(AsmInstr::Oper {
                            opcode: Opcode::Move,
                            dst: target.temp(),
                            src: format!("{offset}(%{rbp})"),
                            jump: None,
                        });
                        return;
                    }
                }
                let src = match *expr {
                    Expr::Mem(_) => {
                        let temp = Temp::new();
                        let (rbp, offset) = Self::access(*expr);
                        self.emit(AsmInstr::Oper {
                            opcode: Opcode::Move,
                            dst: temp.clone(),
                            src: format!("{offset}(%{rbp})"),
                            jump: None,
                        });
                        temp
                    }
                    _ => self.expr(*expr, false),
                };
                let dst = match *target {
                    Expr::Mem(_) | Expr::Binary { .. } => {
                        let (rbp, offset) = Self::access(*target);
                        format!("{offset}(%{rbp})")
                    }
                    _ => target.temp(),
                };
                self.emit(AsmInstr::Oper {
                    opcode: Opcode::Move,
                    dst,
                    src,
                    jump: None,
                });
            }
            Stmt::Label(label) => {
                let id = self.idents.get(&label).copied();
                self.emit(AsmInstr::Oper {
                    opcode: Opcode::Label(label),
                    dst: String::new(),
                    src: String::new(),
                    jump: None,
                });
                if let Some(id) = id {
                    let function = self.functions.get(&id).unwrap();
                    self.emit(AsmInstr::Raw(function.prologue()));
                }
            }
            Stmt::CJump {
                t, op, condition, ..
            } => {
                self.expr(*condition, false);
                self.emit(AsmInstr::Oper {
                    opcode: Opcode::CJump(op.into()),
                    dst: String::new(),
                    src: String::new(),
                    jump: Some(t),
                });
            }
        }
    }

    fn expr(&mut self, expr: Expr, lhs: bool) -> String {
        match expr {
            Expr::Binary { op, left, right } => {
                let right = self.expr(*right, false);
                let left = self.expr(*left, true);
                self.emit(AsmInstr::Oper {
                    opcode: Opcode::from(op),
                    dst: left.clone(),
                    src: right,
                    jump: None,
                });
                left
            }
            Expr::ConstInt(i) => {
                let name = Temp::new();
                self.emit(AsmInstr::Oper {
                    opcode: Opcode::Move,
                    dst: name.clone(),
                    src: format!("${i}"),
                    jump: None,
                });
                name
            }
            Expr::ConstFloat(_) => todo!(),
            Expr::Temp(t) => t,
            Expr::Mem(mem) => {
                let name = Temp::new();
                let src = name.clone();
                let (rbp, offset) = Self::access(*mem);
                let dst = format!("{offset}(%{rbp})");
                let mut oper = AsmInstr::Oper {
                    opcode: Opcode::Move,
                    dst,
                    src,
                    jump: None,
                };
                if lhs {
                    match oper {
                        AsmInstr::Oper {
                            ref mut dst,
                            ref mut src,
                            ..
                        } => {
                            std::mem::swap(dst, src);
                        }
                        _ => unreachable!(),
                    }
                }
                self.emit(oper);
                name
            }
            Expr::Call(name, args) => {
                let args: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.expr(arg, false))
                    .enumerate()
                    .map(|(i, arg)| AsmInstr::Oper {
                        opcode: Opcode::Move,
                        dst: F::registers().argument[i].into(),
                        src: arg,
                        jump: None,
                    })
                    .collect();
                for arg in args {
                    self.emit(arg);
                }
                self.emit(AsmInstr::Call { name });
                format!("%{}", F::registers().ret.value)
            }
            Expr::ESeq { .. } => unreachable!(),
        }
    }

    fn access(mem: Expr) -> (String, i64) {
        let (_, left, right) = match mem {
            Expr::Mem(expr) => expr.binary(),
            Expr::Binary { op, left, right } => (op, left, right),
            _ => unreachable!(),
        };
        (left.temp(), right.int())
    }
}

// TODO: testing
// #[cfg(test)]
// mod tests {
//     use std::collections::HashMap;

//     use crate::{
//         ast,
//         kyir::{arch::amd64::Amd64, canon::Canon, Translator},
//         pass::{SymbolTable, TypeCheckPass},
//         PipelineError, Source,
//     };

//     use super::Codegen;

//     #[test]
//     fn compile() -> Result<(), Box<dyn std::error::Error>> {
//         let source = Source::in_memory(include_str!("other-test.kya").to_string());
//         let ast = ast::Ast::from_source(&source)?;
//         let symbols = SymbolTable::from(&ast.nodes);
//         let mut accesses = HashMap::new();
//         let mut pass = TypeCheckPass::new(&symbols, &mut accesses, source, &ast.nodes);
//         pass.run().map_err(PipelineError::TypeError)?;
//         let mut translator: Translator<Amd64> = Translator::new(&accesses, &symbols);
//         let ir = translator.translate(&ast.nodes);
//         let canon = Canon::new(ir);
//         let ir = canon.canonicalize();
//         let codegen: Codegen<Amd64> = Codegen::new(ir, translator.functions, &ast.nodes);
//         println!("{}", codegen);
//         Ok(())
//     }
// }

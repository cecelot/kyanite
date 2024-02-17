use crate::backend::kyir::ir::{AddressStrategy, BinOp, RelOp};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Opcode {
    Label(String),
    Data { kind: String, value: String },
    Move(AddressStrategy),
    Jump,
    Cmp(RelOp),
    CJump(RelOp),
    Add,
    Sub,
    Mul,
    Div,
    Push,
    Pop,
    Ret,
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

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Opcode::Data { kind, value } => write!(f, ".{kind} {value}"),
            Opcode::Cmp(_) => write!(f, "cmpq"),
            Opcode::Label(label) => write!(f, "{label}:"),
            Opcode::Move(strategy) => write!(
                f,
                "{}",
                match strategy {
                    AddressStrategy::Immediate => "movq",
                    AddressStrategy::Effective => "leaq",
                }
            ),
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
            Opcode::Push => write!(f, "pushq"),
            Opcode::Pop => write!(f, "popq"),
            Opcode::Ret => write!(f, "retq"),
        }
    }
}

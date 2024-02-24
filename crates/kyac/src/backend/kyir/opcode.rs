use crate::backend::kyir::ir::{AddressStrategy, BinOp, RelOp};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Opcode {
    Label(String),
    LoadImmediate,
    LoadEffective((String, String)),
    StoreImmediate,
    StorePair((String, String)),
    LoadPair((String, String)),
    AddTriple((String, String, String)),
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
            Opcode::StorePair((r1, r2)) => write!(f, "stp {r1}, {r2}, [sp, #-16]!"),
            Opcode::LoadPair((r1, r2)) => write!(f, "ldp {r1}, {r2}, [sp], #16"),
            Opcode::LoadEffective((tmp, label)) => write!(
                f,
                "adrp {tmp}, {label}@PAGE\n\tadd {tmp}, {tmp}, {label}@PAGEOFF"
            ),
            Opcode::AddTriple((r1, r2, r3)) => write!(f, "add {r1}, {r2}, {r3}"),
            Opcode::Cmp(_) => write!(f, "cmp"),
            Opcode::LoadImmediate => write!(f, "ldr"),
            Opcode::StoreImmediate => write!(f, "str"),
            Opcode::Label(label) => write!(f, "{label}:"),
            Opcode::Move(_) => write!(f, "mov",),
            Opcode::Jump => write!(f, "b"),
            Opcode::CJump(rel) => {
                let cond = match rel {
                    RelOp::Equal => "eq",
                    RelOp::NotEqual => "ne",
                    RelOp::Less => "lt",
                    RelOp::LessEqual => "le",
                    RelOp::Greater => "gt",
                    RelOp::GreaterEqual => "ge",
                };
                write!(f, "b{cond}")
            }
            Opcode::Add => write!(f, "add"),
            Opcode::Sub => write!(f, "sub"),
            Opcode::Mul => write!(f, "mul"),
            Opcode::Div => write!(f, "sdiv"),
            Opcode::Push => write!(f, "push"),
            Opcode::Pop => write!(f, "pop"),
            Opcode::Ret => write!(f, "ret"),
        }
    }
}

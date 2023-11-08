pub mod llvm;

pub use llvm::Ir;
pub use llvm::IrError;

#[allow(dead_code)]
pub enum AssemInstr {
    Oper {
        opcode: String,
        dst: Vec<String>,
        src: Vec<String>,
        jump: Option<String>,
    },
    Move {
        opcode: String,
        dst: String,
        src: String,
    },
}

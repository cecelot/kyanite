use crate::{
    backend::llvm::{Ir, IrError},
    builtins,
};

pub struct Builtins;

impl Builtins {
    pub fn inject(ir: &mut Ir<'_, '_>) -> Result<(), IrError> {
        for node in &mut builtins::builtins().nodes {
            ir.decl(node)?;
        }
        Ok(())
    }
}

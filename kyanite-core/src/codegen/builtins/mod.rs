use crate::{ast::Ast, codegen::Ir};

use super::IrError;

pub struct Builtins {}

impl Builtins {
    pub fn new(ir: &mut Ir<'_, '_>) -> Result<Self, IrError> {
        let mut ast = Ast::from_string(include_str!("stub.kya").to_string()).unwrap();
        for node in &mut ast.file.nodes {
            ir.toplevel(node)?;
        }
        Ok(Self {})
    }
}

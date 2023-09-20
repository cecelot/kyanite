use self::{math::Math, println::Println};
use crate::codegen::Ir;

mod math;
mod println;

pub trait Builtin {
    fn build(&self, ir: &mut Ir<'_, '_>);
}

pub struct Builtins {}

impl Builtins {
    pub fn new() -> Self {
        Self {}
    }

    pub fn build(&mut self, ir: &mut Ir<'_, '_>) {
        Println {}.build(ir);
        Math {}.build(ir);
    }
}

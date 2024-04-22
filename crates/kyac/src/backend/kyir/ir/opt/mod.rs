pub(in crate::backend::kyir) mod quadruple;

use crate::backend::kyir::ir::{opt::quadruple::Quadruple, Stmt};

pub fn optimize(initial: Vec<Stmt>) -> Vec<Stmt> {
    let simplified: Vec<_> = initial.into_iter().map(Quadruple::quadruple).collect();
    simplified
}

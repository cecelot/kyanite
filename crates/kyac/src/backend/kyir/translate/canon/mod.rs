mod blocks;
mod eseq;
mod rewrite;
mod trace;

use crate::backend::kyir::{
    ir::Move,
    translate::{
        canon::{blocks::BasicBlocks, eseq::ESeqs, rewrite::Rewrite},
        Expr, Stmt,
    },
};
use std::collections::VecDeque;

pub fn canonicalize(ir: Vec<Stmt>) -> Vec<Stmt> {
    let ir: Vec<_> = ir
        .into_iter()
        .map(|item| item.rewrite(false))
        .filter(|item| !matches!(item, Stmt::Noop))
        .collect();
    let mut new = vec![];
    let mut replacements = vec![];
    for item in ir {
        let name = item.label();
        let mut body = vec![];
        item.extract(&mut body, &mut replacements);
        new.push((name, body));
    }
    for (search, temp) in replacements {
        for (_, item) in &mut new {
            for stmt in item.iter_mut() {
                stmt.replace(search, &temp);
            }
        }
    }
    let blocks = BasicBlocks::new(new);
    trace::new(VecDeque::from(blocks.inner()))
}

pub trait Extract {
    fn extract(self, ir: &mut Vec<Stmt>, replacements: &mut Vec<(usize, Box<Expr>)>);
}

impl Extract for Stmt {
    fn extract(self, ir: &mut Vec<Stmt>, replacements: &mut Vec<(usize, Box<Expr>)>) {
        match self {
            Stmt::Seq(seq) => {
                seq.left.extract(ir, replacements);
                if let Some(right) = seq.right {
                    right.extract(ir, replacements);
                }
            }
            Stmt::Move(m) => {
                update(&m.expr, ir, replacements);
                update(&m.target, ir, replacements);
                ir.push(Move::wrapped(*m.target.clone(), *m.expr.clone()));
            }
            Stmt::Expr(e) => {
                update(&e, ir, replacements);
                ir.push(Stmt::Expr(e));
            }
            Stmt::CJump(cjmp) => {
                update(&cjmp.condition, ir, replacements);
                ir.push(Stmt::CJump(cjmp));
            }
            Stmt::Label(_) | Stmt::Noop | Stmt::Jump(_) => ir.push(self),
        }
    }
}

fn update(expr: &Expr, ir: &mut Vec<Stmt>, replacements: &mut Vec<(usize, Box<Expr>)>) {
    let mut nested = vec![];
    expr.eseqs(&mut nested);
    for expr in nested.iter().rev() {
        let Expr::ESeq(eseq) = expr else {
            panic!("Expected `Expr::ESeq`")
        };
        if let Stmt::Seq(seq) = *eseq.stmt.clone() {
            ir.push(*seq.left);
            if let Some(right) = seq.right {
                ir.push(*right);
            }
        } else {
            ir.push(*eseq.stmt.clone());
        }
        replacements.push((eseq.id, eseq.expr.clone()));
    }
}

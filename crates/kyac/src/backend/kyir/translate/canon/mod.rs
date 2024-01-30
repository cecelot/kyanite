mod blocks;
mod eseq;
mod rewrite;
mod trace;

use crate::backend::kyir::translate::{
    canon::{blocks::BasicBlocks, eseq::ESeqs, rewrite::Rewrite},
    Expr, Stmt,
};
use std::collections::VecDeque;

pub fn canonicalize(mut ir: Vec<Stmt>) -> Vec<Stmt> {
    ir = ir
        .into_iter()
        .map(|item| item.rewrite(false, false))
        .collect();
    ir.retain(|item| !matches!(item, Stmt::Noop));
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
            Stmt::Seq { left, right } => {
                left.extract(ir, replacements);
                if let Some(right) = right {
                    right.extract(ir, replacements);
                }
            }
            Stmt::Move { expr, target } => {
                update(&expr, ir, replacements);
                ir.push(Stmt::Move {
                    target: target.clone(),
                    expr: expr.clone(),
                });
            }
            Stmt::Label(_) | Stmt::Noop | Stmt::Jump(_) | Stmt::Expr(_) => ir.push(self),
            Stmt::CJump {
                condition,
                op,
                t,
                f,
            } => {
                update(&condition, ir, replacements);
                ir.push(Stmt::CJump {
                    condition,
                    op,
                    t,
                    f,
                });
            }
        }
    }
}

fn update(expr: &Expr, ir: &mut Vec<Stmt>, replacements: &mut Vec<(usize, Box<Expr>)>) {
    let mut nested = vec![];
    expr.eseqs(&mut nested);
    for expr in nested.iter().rev() {
        if let Expr::ESeq {
            stmt,
            expr: temp,
            id: search,
        } = expr
        {
            if let Stmt::Seq { left, right } = *stmt.clone() {
                ir.push(*left);
                if let Some(right) = right {
                    ir.push(*right);
                }
            } else {
                ir.push(*stmt.clone());
            }
            replacements.push((*search, temp.clone()));
        } else {
            panic!("Expected `Expr::ESeq`")
        }
    }
}

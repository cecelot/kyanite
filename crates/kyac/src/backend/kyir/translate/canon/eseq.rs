use crate::backend::kyir::ir::{Expr, Stmt};

pub trait ESeqs<'a> {
    fn eseqs(&'a self, list: &mut Vec<&'a Expr>);
    fn replace(&'a mut self, search: usize, temp: &Expr);
}

impl<'a> ESeqs<'a> for Expr {
    fn replace(&'a mut self, search: usize, temp: &Expr) {
        match self {
            Expr::Binary(bin) => {
                bin.left.replace(search, temp);
                bin.right.replace(search, temp);
            }
            Expr::ESeq(eseq) => {
                if search == eseq.id {
                    *self = temp.clone();
                }
            }
            Expr::Mem(mem) => {
                mem.expr.replace(search, temp);
            }
            Expr::Call(call) => {
                for arg in &mut call.args {
                    arg.replace(search, temp);
                }
            }
            Expr::ConstInt(_)
            | Expr::ConstFloat(_)
            | Expr::Temp(_)
            | Expr::Dereferenced(_)
            | Expr::ConstStr(_) => {}
        }
    }

    fn eseqs(&'a self, list: &mut Vec<&'a Expr>) {
        match self {
            Expr::Binary(bin) => {
                bin.left.eseqs(list);
                bin.right.eseqs(list);
            }
            Expr::ESeq(eseq) => {
                list.push(self);
                eseq.stmt.eseqs(list);
                eseq.expr.eseqs(list);
            }
            Expr::Mem(mem) => {
                mem.expr.eseqs(list);
            }
            Expr::Call(call) => {
                for arg in &call.args {
                    arg.eseqs(list);
                }
            }
            Expr::ConstInt(_)
            | Expr::ConstFloat(_)
            | Expr::Temp(_)
            | Expr::Dereferenced(_)
            | Expr::ConstStr(_) => {}
        }
    }
}

impl<'a> ESeqs<'a> for Stmt {
    fn replace(&'a mut self, search: usize, temp: &Expr) {
        match self {
            Stmt::Expr(e) => {
                e.replace(search, temp);
            }
            Stmt::Seq(seq) => {
                seq.left.replace(search, temp);
                if let Some(right) = &mut seq.right {
                    right.replace(search, temp);
                }
            }
            Stmt::Jump(_) | Stmt::Label(_) | Stmt::Noop => {}
            Stmt::CJump(cjmp) => {
                cjmp.condition.replace(search, temp);
            }
            Stmt::Move(m) => {
                m.expr.replace(search, temp);
                m.target.replace(search, temp);
            }
        }
    }

    fn eseqs(&'a self, list: &mut Vec<&'a Expr>) {
        match self {
            Stmt::Expr(e) => {
                e.eseqs(list);
            }
            Stmt::Seq(seq) => {
                seq.left.eseqs(list);
                if let Some(right) = &seq.right {
                    right.eseqs(list);
                }
            }
            Stmt::Jump(_) | Stmt::Label(_) | Stmt::Noop => {}
            Stmt::CJump(cjmp) => {
                cjmp.condition.eseqs(list);
            }
            Stmt::Move(m) => {
                m.expr.eseqs(list);
                m.target.eseqs(list);
            }
        }
    }
}

use super::{Expr, Stmt};

pub trait ESeqs<'a> {
    fn eseqs(&'a self, list: &mut Vec<&'a Expr>);
    fn replace(&'a mut self, search: usize, temp: &Expr);
}

impl<'a> ESeqs<'a> for Expr {
    fn replace(&'a mut self, search: usize, temp: &Expr) {
        match self {
            Expr::Binary { left, right, .. } => {
                left.replace(search, temp);
                right.replace(search, temp);
            }
            Expr::ConstInt(_) | Expr::ConstFloat(_) | Expr::Temp(_) => {}
            Expr::ESeq { id, .. } => {
                if search == *id {
                    *self = temp.clone();
                }
            }
            Expr::Mem(expr) => {
                expr.replace(search, temp);
            }
            Expr::Call(_, args) => {
                for arg in args {
                    arg.replace(search, temp);
                }
            }
        }
    }

    fn eseqs(&'a self, list: &mut Vec<&'a Expr>) {
        match self {
            Expr::Binary { left, right, .. } => {
                left.eseqs(list);
                right.eseqs(list);
            }
            Expr::ESeq { stmt, expr, .. } => {
                list.push(self);
                stmt.eseqs(list);
                expr.eseqs(list);
            }
            Expr::Mem(expr) => {
                expr.eseqs(list);
            }
            Expr::Call(_, args) => {
                for arg in args {
                    arg.eseqs(list);
                }
            }
            Expr::ConstInt(_) | Expr::ConstFloat(_) | Expr::Temp(_) => {}
        }
    }
}

impl<'a> ESeqs<'a> for Stmt {
    fn replace(&'a mut self, search: usize, temp: &Expr) {
        match self {
            Stmt::Expr(e) => {
                e.replace(search, temp);
            }
            Stmt::Seq { left, right } => {
                left.replace(search, temp);
                if let Some(right) = right {
                    right.replace(search, temp);
                }
            }
            Stmt::Jump(_) | Stmt::Label(_) | Stmt::Noop => {}
            Stmt::CJump { condition, .. } => {
                condition.replace(search, temp);
            }
            Stmt::Move { target, expr } => {
                expr.replace(search, temp);
                target.replace(search, temp);
            }
        }
    }

    fn eseqs(&'a self, list: &mut Vec<&'a Expr>) {
        match self {
            Stmt::Expr(e) => {
                e.eseqs(list);
            }
            Stmt::Seq { left, right } => {
                left.eseqs(list);
                if let Some(right) = right {
                    right.eseqs(list);
                }
            }
            Stmt::Jump(_) | Stmt::Label(_) | Stmt::Noop => {}
            Stmt::CJump { condition, .. } => {
                condition.eseqs(list);
            }
            Stmt::Move { target, expr } => {
                expr.eseqs(list);
                target.eseqs(list);
            }
        }
    }
}

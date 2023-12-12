// Yes, this is very much a hack.
use std::rc::Rc;

use super::{Decl, Expr, Stmt};

pub trait StripId {
    fn strip_id(&mut self);
}

impl StripId for Decl {
    fn strip_id(&mut self) {
        if let Self::Function(f) = self {
            let func = Rc::get_mut(f).unwrap();
            func.id = 0;
            for stmt in &mut func.body {
                stmt.strip_id();
            }
        }
    }
}

impl StripId for Stmt {
    fn strip_id(&mut self) {
        match self {
            Self::Assign(a) => {
                let assign = Rc::get_mut(a).unwrap();
                assign.target.strip_id();
                assign.expr.strip_id();
            }
            Self::Expr(Expr::Call(c)) => {
                let call = Rc::get_mut(c).unwrap();
                for arg in &mut call.args {
                    arg.strip_id();
                }
            }
            _ => {}
        }
    }
}

impl StripId for Expr {
    fn strip_id(&mut self) {
        match self {
            Self::Access(a) => {
                let access = Rc::get_mut(a).unwrap();
                access.chain.iter_mut().for_each(|el| el.strip_id());
                access.id = 0;
            }
            Self::Call(c) => {
                let call = Rc::get_mut(c).unwrap();
                call.args.iter_mut().for_each(|arg| arg.strip_id());
            }
            Self::Binary(b) => {
                let binary = Rc::get_mut(b).unwrap();
                binary.left.strip_id();
                binary.right.strip_id();
            }
            Self::Init(i) => {
                let init = Rc::get_mut(i).unwrap();
                for initializer in &mut init.initializers {
                    initializer.expr.strip_id();
                }
            }
            Self::Unary(u) => {
                let unary = Rc::get_mut(u).unwrap();
                unary.expr.strip_id();
            }
            _ => {}
        }
    }
}

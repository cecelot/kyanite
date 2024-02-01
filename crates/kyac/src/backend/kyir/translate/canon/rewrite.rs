use crate::backend::kyir::{
    ir::Move,
    translate::{Binary, Call, ESeq, Seq},
    Expr, Stmt, Temp,
};

/// Rewrite any [`Expr::Call`] to [`Expr::ESeq`] if it is not
/// the immediate child of a [`Stmt::Move`] or [`Stmt::Expr`]
pub trait Rewrite<R> {
    fn rewrite(self, immediate: bool, child: bool) -> R;
}

pub trait Substitute {
    fn substitute(&mut self, substitutions: &[(String, String)]);
}

impl Rewrite<Vec<Expr>> for Vec<Expr> {
    fn rewrite(self, immediate: bool, child: bool) -> Vec<Expr> {
        self.into_iter()
            .map(|arg| match arg {
                Expr::Call { .. } => {
                    let temp = Temp::next();
                    ESeq::wrapped(
                        Move::wrapped(Temp::wrapped(temp.clone()), arg.rewrite(immediate, child)),
                        Temp::wrapped(temp),
                    )
                }
                _ => arg,
            })
            .collect()
    }
}

impl Substitute for Vec<Expr> {
    fn substitute(&mut self, substitutions: &[(String, String)]) {
        for arg in self.iter_mut() {
            arg.substitute(substitutions);
        }
    }
}

impl Rewrite<Expr> for Expr {
    fn rewrite(self, immediate: bool, child: bool) -> Expr {
        match self {
            Expr::Call(call) => {
                let args = call.args.clone().rewrite(false, false);
                if immediate {
                    Expr::Call(call)
                } else {
                    let temp = Temp::next();
                    ESeq::wrapped(
                        Seq::wrapped(
                            Move::wrapped(
                                Temp::wrapped(temp.clone()),
                                Call::wrapped(call.name, args),
                            ),
                            None,
                        ),
                        Temp::wrapped(temp),
                    )
                }
            }
            Expr::Binary(bin) => {
                let left = bin.left.rewrite(false, false);
                let right = bin.right.rewrite(false, false);
                let bin = Binary::wrapped(bin.op, left, right);
                if immediate && child {
                    let temp = Temp::next();
                    ESeq::wrapped(
                        Seq::wrapped(Move::wrapped(Temp::wrapped(temp.clone()), bin), None),
                        Temp::wrapped(temp),
                    )
                } else {
                    bin
                }
            }
            _ => self,
        }
    }
}

impl Substitute for Expr {
    fn substitute(&mut self, substitutions: &[(String, String)]) {
        if let Expr::ESeq(eseq) = self {
            eseq.stmt.substitute(substitutions);
        }
    }
}

impl Rewrite<Stmt> for Stmt {
    fn rewrite(self, _: bool, _: bool) -> Stmt {
        match self {
            Stmt::Expr(e) => Stmt::Expr(Box::new(e.rewrite(true, false))),
            Stmt::Seq(seq) => Seq::wrapped(
                seq.left.rewrite(false, false),
                seq.right.map(|item| item.rewrite(false, false)),
            ),
            Stmt::Move(m) => Move::wrapped(*m.target, m.expr.rewrite(true, true)),
            Stmt::CJump(cjmp) => Stmt::CJump(cjmp),
            _ => self,
        }
    }
}

impl Substitute for Stmt {
    fn substitute(&mut self, substitutions: &[(String, String)]) {
        match self {
            Stmt::Seq(seq) => {
                seq.left.substitute(substitutions);
                if let Some(right) = &mut seq.right {
                    right.substitute(substitutions);
                }
            }
            Stmt::Move(m) => {
                m.target.substitute(substitutions);
            }
            Stmt::CJump { .. } | Stmt::Label(_) | Stmt::Noop => {}
            Stmt::Expr(expr) => expr.substitute(substitutions),
            Stmt::Jump(jmp) => {
                for (from, to) in substitutions {
                    if jmp.target == *from {
                        jmp.target = to.clone();
                    }
                }
            }
        }
    }
}

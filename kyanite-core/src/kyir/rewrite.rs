use super::{Expr, Stmt, Temp};

/// Rewrite any [`Expr::Call`] to [`Expr::ESeq`] if it is not
/// the immediate child of a [`Stmt::Move`] or [`Stmt::Expr`]
pub trait Rewrite<R> {
    fn rewrite(self, immediate: bool) -> R;
}

pub trait Substitute {
    fn substitute(&mut self, substitutions: &[(String, String)]);
}

impl Rewrite<Vec<Expr>> for Vec<Expr> {
    fn rewrite(self, immediate: bool) -> Vec<Expr> {
        self.into_iter()
            .map(|arg| match arg {
                Expr::Call(..) => {
                    let temp = Temp::new();
                    Expr::eseq(
                        Box::new(Stmt::Move {
                            target: Box::new(Expr::Temp(temp.clone())),
                            expr: Box::new(arg.rewrite(immediate)),
                        }),
                        Box::new(Expr::Temp(temp)),
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
    fn rewrite(self, immediate: bool) -> Expr {
        match self {
            Expr::Call(name, args) => {
                let args = args.rewrite(false);
                if !immediate {
                    let temp = Temp::new();
                    Expr::eseq(
                        Box::new(Stmt::Seq {
                            left: Box::new(Stmt::Move {
                                target: Box::new(Expr::Temp(temp.clone())),
                                expr: Box::new(Expr::Call(name, args)),
                            }),
                            right: None,
                        }),
                        Box::new(Expr::Temp(temp)),
                    )
                } else {
                    Expr::Call(name, args)
                }
            }
            Expr::Binary { left, right, op } => {
                let left = left.rewrite(false);
                let right = right.rewrite(false);
                Expr::Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    op,
                }
            }
            _ => self,
        }
    }
}

impl Substitute for Expr {
    fn substitute(&mut self, substitutions: &[(String, String)]) {
        match self {
            Expr::Temp(_) => {}
            Expr::ConstInt(_) => {}
            Expr::ConstFloat(_) => {}
            Expr::Mem(_) => {}
            Expr::Call(..) => {}
            Expr::Binary { .. } => {}
            Expr::ESeq { stmt, .. } => {
                stmt.substitute(substitutions);
            }
        }
    }
}

impl Rewrite<Stmt> for Stmt {
    fn rewrite(self, _: bool) -> Stmt {
        match self {
            Stmt::Expr(e) => Stmt::Expr(Box::new(e.rewrite(true))),
            Stmt::Seq { left, right } => Stmt::Seq {
                left: Box::new(left.rewrite(false)),
                right: right.map(|item| Box::new(item.rewrite(false))),
            },
            Stmt::Move { target, expr } => Stmt::Move {
                target,
                expr: Box::new(expr.rewrite(true)),
            },
            Stmt::CJump {
                left,
                right,
                t,
                f,
                op,
            } => {
                let l = Temp::new();
                let left = Box::new(Expr::eseq(
                    Box::new(Stmt::Move {
                        target: Box::new(Expr::Temp(l.clone())),
                        expr: Box::new(left.rewrite(false)),
                    }),
                    Box::new(Expr::Temp(l)),
                ));
                Stmt::CJump {
                    left,
                    right,
                    t,
                    f,
                    op,
                }
            }
            _ => self,
        }
    }
}

impl Substitute for Stmt {
    fn substitute(&mut self, substitutions: &[(String, String)]) {
        match self {
            Stmt::Seq { left, right } => {
                left.substitute(substitutions);
                if let Some(right) = right {
                    right.substitute(substitutions);
                }
            }
            Stmt::Move { target, .. } => {
                target.substitute(substitutions);
            }
            Stmt::CJump { .. } => {}
            Stmt::Expr(expr) => expr.substitute(substitutions),
            Stmt::Label(_) => {}
            Stmt::Noop => {}
            Stmt::Jump(j) => {
                for (from, to) in substitutions {
                    if j == from {
                        *j = to.clone();
                    }
                }
            }
        }
    }
}

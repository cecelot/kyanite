use crate::backend::kyir::ir::{Binary, CJump, Call, ESeq, Expr, Move, Seq, Stmt, Temp};

pub fn verify(stmt: &Stmt) {
    match stmt {
        Stmt::Move(m)
            if matches!(*m.target, Expr::Temp(_)) && matches!(*m.expr, Expr::Binary(_)) => {} // #1
        Stmt::Move(m)
            if matches!(*m.target, Expr::Temp(_))
                && matches!(
                    *m.expr,
                    Expr::Temp(_) | Expr::ConstInt(_) | Expr::ConstFloat(_)
                ) => {} // #2
        Stmt::Move(m) if matches!(*m.target, Expr::Temp(_)) && matches!(*m.expr, Expr::Mem(_)) => {} // #3
        Stmt::Move(m)
            if matches!(*m.target, Expr::Mem(_))
                && matches!(
                    *m.expr,
                    Expr::Temp(_) | Expr::ConstInt(_) | Expr::ConstFloat(_)
                ) => {} // #4
        Stmt::Expr(e) if matches!(**e, Expr::Call(_)) => {} // #4
        Stmt::Expr(e) if matches!(**e, Expr::Mem(_)) => {}  // #5
        Stmt::Move(m) if matches!(*m.target, Expr::Temp(_)) && matches!(*m.expr, Expr::Call(_)) => {
        } // #6
        Stmt::Jump(_) /* #7 */ | Stmt::Label(_) /* #8 */ | Stmt::CJump(_) /* #9 */ | Stmt::Noop => {}
        _ => panic!("Invalid quadruple: {stmt:?}"),
    }
}

pub trait Quadruple {
    fn quadruple(self) -> Self;
}

pub trait Flatten {
    fn flatten(self) -> Vec<Self>
    where
        Self: Sized;
}

impl Flatten for Stmt {
    fn flatten(self) -> Vec<Self> {
        match self {
            Stmt::Seq(seq) => {
                let mut left = seq.left.flatten();
                let mut right = seq.right.map_or_else(Vec::new, |r| r.flatten());
                left.append(&mut right);
                left
            }
            _ => vec![self],
        }
    }
}

impl Quadruple for Stmt {
    fn quadruple(self) -> Self {
        match self {
            Self::CJump(cjmp) => Self::CJump(CJump {
                condition: Box::new(cjmp.condition.quadruple()),
                ..cjmp
            }),
            Self::Expr(expr) => Stmt::Expr(Box::new(expr.quadruple())),
            Self::Seq(seq) => Seq::wrapped(
                seq.left.quadruple(),
                seq.right.map(|right| right.quadruple()),
            ),
            Self::Move(m) => Move::wrapped(m.target.quadruple(), m.expr.quadruple()),
            _ => self,
        }
    }
}

impl Quadruple for Expr {
    fn quadruple(self) -> Self {
        match self {
            Self::ESeq(eseq) => ESeq::wrapped(eseq.stmt.quadruple(), eseq.expr.quadruple()),
            Self::Call(call) => Call::wrapped(
                call.name,
                call.args.into_iter().map(Quadruple::quadruple).collect(),
            ),
            Self::Binary(bin) => {
                let wrap = |side: Box<Expr>| {
                    if matches!(*side, Expr::Binary(_)) {
                        let tmp = Temp::next();
                        ESeq::wrapped(
                            Move::wrapped(Temp::wrapped(tmp.clone()), side.quadruple()),
                            Temp::wrapped(tmp),
                        )
                    } else {
                        side.quadruple()
                    }
                };
                let tmp = Temp::next();
                let bin = Binary::wrapped(bin.op, wrap(bin.left), wrap(bin.right));
                ESeq::wrapped(
                    Move::wrapped(Temp::wrapped(tmp.clone()), bin),
                    Temp::wrapped(tmp),
                )
            }
            _ => self,
        }
    }
}

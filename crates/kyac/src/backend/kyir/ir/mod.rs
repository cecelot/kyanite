use crate::token::Kind;

#[allow(clippy::wildcard_imports)]
pub use self::node::*;

mod node;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    ConstInt(Const<i64>),
    ConstFloat(Const<f64>),
    Temp(Temp),
    Binary(Binary),
    Mem(Mem),
    Call(Call),
    ESeq(ESeq),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Move(Move),
    Expr(Box<Expr>),
    Label(Label),
    Seq(Seq),
    Jump(Jump),
    CJump(CJump),
    Noop,
}

impl From<&[Stmt]> for Stmt {
    fn from(stmts: &[Stmt]) -> Self {
        match stmts.len() {
            0 => Stmt::Noop,
            1 => stmts[0].clone(),
            _ => Seq::wrapped(stmts[0].clone(), Some(Stmt::from(&stmts[1..]))),
        }
    }
}

/// Returns a new `Expr` that loads the value at the memory location specified by `expr`
/// and stores it in a temporary. This is required because (a) moves from memory address to memory
/// address are unsupported and (b) binary operations require at least one of their operands to be
/// in registers.
fn wrap_memory_load(expr: Expr) -> Expr {
    let temp = Temp::next();
    ESeq::wrapped(
        Move::wrapped(Temp::wrapped(temp.clone()), expr),
        Temp::wrapped(temp),
    )
}

impl Expr {
    /// Returns a new `Expr` where `left` and `right` are not both memory addresses.
    pub fn checked_binary(op: BinOp, left: Expr, right: Expr) -> Self {
        let right = match (&left, right) {
            (Expr::Mem(_), expr @ Expr::Mem(_)) => wrap_memory_load(expr),
            (Expr::Mem(_), expr @ Expr::ESeq { .. }) => {
                if matches!(expr, Expr::Mem(_)) {
                    wrap_memory_load(expr)
                } else {
                    expr
                }
            }
            (_, right) => right,
        };
        Binary::wrapped(op, left, right)
    }
}

impl Stmt {
    /// Returns a new `Stmt` where `target` and `expr` are not both memory addresses.
    pub fn checked_move(target: Expr, expr: Expr) -> Self {
        let expr = match (&target, expr) {
            (Expr::Mem(_), expr @ Expr::Mem(_)) => wrap_memory_load(expr),
            (Expr::Mem(_), expr @ Expr::ESeq { .. }) => {
                if matches!(expr, Expr::Mem(_)) {
                    wrap_memory_load(expr)
                } else {
                    expr
                }
            }
            (_, expr) => expr,
        };
        Move::wrapped(target, expr)
    }
}

impl Expr {
    pub fn relation(&self) -> Option<RelOp> {
        match self {
            Self::Binary(Binary {
                op: BinOp::Cmp(rel),
                ..
            }) => Some(*rel),
            Self::ConstInt(_) => Some(RelOp::Equal),
            _ => None,
        }
    }

    pub fn temp(&self) -> Option<String> {
        match self {
            Self::Temp(t) => Some(t.name.clone()),
            _ => None,
        }
    }

    pub fn int(&self) -> Option<i64> {
        match self {
            Self::ConstInt(i) => Some(i.value),
            _ => None,
        }
    }

    pub fn binary(&self) -> Option<&Binary> {
        match self {
            Self::Binary(bin) => Some(bin),
            _ => None,
        }
    }
}

impl Stmt {
    pub fn label(&self) -> String {
        match self {
            Self::Seq(Seq { left, .. }) => left.label(),
            Self::Label(label) => label.clone().name,
            _ => panic!("called `Stmt::label()` on a non-label statement"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    Xor,
    Cmp(RelOp),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum RelOp {
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

impl From<Kind> for BinOp {
    fn from(kind: Kind) -> Self {
        match kind {
            Kind::Plus => BinOp::Plus,
            Kind::Minus => BinOp::Minus,
            Kind::Star => BinOp::Mul,
            Kind::Slash => BinOp::Div,
            Kind::BangEqual => BinOp::Cmp(RelOp::NotEqual),
            Kind::EqualEqual => BinOp::Cmp(RelOp::Equal),
            Kind::Greater => BinOp::Cmp(RelOp::Greater),
            Kind::GreaterEqual => BinOp::Cmp(RelOp::GreaterEqual),
            Kind::Less => BinOp::Cmp(RelOp::Less),
            Kind::LessEqual => BinOp::Cmp(RelOp::LessEqual),
            _ => unreachable!("not a valid binary operator"),
        }
    }
}

impl From<BinOp> for RelOp {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Cmp(rel) => rel,
            _ => panic!("Cannot convert {value:?} to RelOp"),
        }
    }
}

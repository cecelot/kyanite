use crate::backend::kyir::ir::{BinOp, Expr, Stmt};
use std::{
    ops::Deref,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Move {
    pub target: Box<Expr>,
    pub expr: Box<Expr>,
}

impl Move {
    pub fn wrapped(target: Expr, expr: Expr) -> Stmt {
        Stmt::Move(Self {
            target: Box::new(target),
            expr: Box::new(expr),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Seq {
    pub left: Box<Stmt>,
    pub right: Option<Box<Stmt>>,
}

impl Seq {
    pub fn wrapped(left: Stmt, right: Option<Stmt>) -> Stmt {
        Stmt::Seq(Self {
            left: Box::new(left),
            right: right.map(Box::new),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Jump {
    pub target: String,
}

impl Jump {
    pub fn wrapped(target: String) -> Stmt {
        Stmt::Jump(Self { target })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CJump {
    pub op: BinOp,
    pub condition: Box<Expr>,
    pub t: String,
    pub f: String,
}

impl CJump {
    pub fn wrapped(op: BinOp, condition: Expr, t: String, f: String) -> Stmt {
        Stmt::CJump(Self {
            op,
            condition: Box::new(condition),
            t,
            f,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Const<T> {
    pub value: T,
}

impl<T> Const<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }

    pub fn int(value: i64) -> Expr {
        Expr::ConstInt(Const::new(value))
    }

    pub fn float(value: f64) -> Expr {
        Expr::ConstFloat(Const::new(value))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Temp {
    pub name: String,
}

impl Deref for Temp {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl Temp {
    pub fn next() -> String {
        static ID: AtomicUsize = AtomicUsize::new(0);
        format!("T{}", ID.fetch_add(1, Ordering::SeqCst))
    }

    pub fn wrapped(name: String) -> Expr {
        Expr::Temp(Self { name })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub name: String,
}

impl Label {
    pub fn next() -> String {
        static ID: AtomicUsize = AtomicUsize::new(0);
        format!("L{}", ID.fetch_add(1, Ordering::SeqCst))
    }

    pub fn wrapped(name: String) -> Stmt {
        Stmt::Label(Self { name })
    }
}

impl Deref for Label {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl Binary {
    pub fn wrapped(op: BinOp, left: Expr, right: Expr) -> Expr {
        Expr::Binary(Self {
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Mem {
    pub expr: Box<Expr>,
}

impl Mem {
    pub fn wrapped(expr: Expr) -> Expr {
        Expr::Mem(Self {
            expr: Box::new(expr),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub name: String,
    pub args: Vec<Expr>,
}

impl Call {
    pub fn wrapped(name: String, args: Vec<Expr>) -> Expr {
        Expr::Call(Self { name, args })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ESeq {
    pub stmt: Box<Stmt>,
    pub expr: Box<Expr>,
    pub id: usize,
}

impl ESeq {
    pub fn wrapped(stmt: Stmt, expr: Expr) -> Expr {
        static ID: AtomicUsize = AtomicUsize::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        Expr::ESeq(Self {
            stmt: Box::new(stmt),
            expr: Box::new(expr),
            id,
        })
    }
}

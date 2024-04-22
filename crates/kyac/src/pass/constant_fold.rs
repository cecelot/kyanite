use std::ops::{Add, Div, Mul, Sub};

use crate::{
    ast::{
        node::{self, Literal},
        Expr,
    },
    token::{Kind, Span, Token},
};

impl node::Binary {
    pub fn fold(&self) -> Expr {
        let ignored =
            || node::Binary::wrapped(*self.left.clone(), self.op.clone(), *self.right.clone());
        let token = Token::new(Kind::Literal, None, Span::default());
        match (&*self.left, &*self.right) {
            (Expr::Int(left), Expr::Int(right)) => {
                Literal::<i64>::int(apply(left.value, right.value, self.op.kind), token)
            }
            (Expr::Float(left), Expr::Float(right)) => {
                Literal::<f64>::float(apply(left.value, right.value, self.op.kind), token)
            }
            (Expr::Binary(left), Expr::Int(right)) => {
                let left = left.fold();
                match left {
                    Expr::Int(literal) => {
                        Literal::<i64>::int(apply(literal.value, right.value, self.op.kind), token)
                    }
                    _ => ignored(),
                }
            }
            (Expr::Int(left), Expr::Binary(right)) => {
                let right = right.fold();
                match right {
                    Expr::Int(literal) => {
                        Literal::<i64>::int(apply(left.value, literal.value, self.op.kind), token)
                    }
                    _ => ignored(),
                }
            }
            (Expr::Binary(left), Expr::Binary(right)) => {
                let left = left.fold();
                let right = right.fold();
                match (&left, &right) {
                    (Expr::Int(left), Expr::Int(right)) => Literal::<i64>::int(
                        apply(left.value, right.value, self.op.kind),
                        Token::new(Kind::Literal, None, Span::default()),
                    ),
                    (Expr::Float(left), Expr::Float(right)) => Literal::<f64>::float(
                        apply(left.value, right.value, self.op.kind),
                        Token::new(Kind::Literal, None, Span::default()),
                    ),
                    _ => node::Binary::wrapped(left, self.op.clone(), right),
                }
            }
            _ => ignored(),
        }
    }
}

fn apply<T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T>>(
    left: T,
    right: T,
    op: Kind,
) -> T {
    match op {
        Kind::Plus => left + right,
        Kind::Minus => left - right,
        Kind::Star => left * right,
        Kind::Slash => left / right,
        _ => unreachable!(),
    }
}

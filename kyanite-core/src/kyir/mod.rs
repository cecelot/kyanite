use std::collections::HashMap;

use crate::{
    ast::Expr as AstExpr,
    ast::{Decl as AstDecl, Stmt as AstStmt},
    token::TokenKind,
};

use self::arch::Frame;

mod arch;

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    // And,
    // LShift,
    // RShift,
    // ARShift,
    Xor,
    Cmp(RelOp),
}

#[derive(Debug, Copy, Clone)]
pub enum RelOp {
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug, Clone)]
pub enum Expr {
    ConstInt(i64),
    ConstFloat(f64),
    Temp(String),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Mem(Box<Expr>),
    Call(String, Vec<Expr>),
    // logical comparison requires ESeq
    // ESeq(Stmt, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Moves `expr` into the temporary specified by `target` (an [`Expr::Temp`])
    MoveTemp {
        target: Box<Expr>,
        expr: Box<Expr>,
    },
    /// Moves `expr` into the location in the stack frame specified by `target` (a [`Expr::Binary`])
    MoveMem {
        target: Box<Expr>,
        expr: Box<Expr>,
    },
    Expr(Box<Expr>),
    Label(String),
    /// Evaluates `left`, then evaluates `right`
    Seq {
        left: Box<Stmt>,
        right: Option<Box<Stmt>>,
    },
    Jump(String),
    Noop,
    CJump {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
        t: String,
        f: String,
    },
}

impl From<&[Stmt]> for Stmt {
    fn from(stmts: &[Stmt]) -> Self {
        match stmts.len() {
            0 => Stmt::Noop,
            1 => stmts[0].clone(),
            _ => Stmt::Seq {
                left: Box::new(stmts[0].clone()),
                right: Some(Box::new(Stmt::from(&stmts[1..]))),
            },
        }
    }
}

pub struct Translator<F: Frame> {
    functions: HashMap<usize, F>,
    function: Option<usize>,
    labels: usize,
}

impl<F: Frame> Translator<F> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            function: None,
            labels: 0,
        }
    }

    #[allow(dead_code)]
    pub fn translate(&mut self, ast: &[AstDecl]) -> Vec<Stmt> {
        ast.iter().map(|decl| decl.translate(self)).collect()
    }

    fn label(&mut self) -> String {
        let label = format!("L{}", self.labels);
        self.labels += 1;
        label
    }
}

trait Translate<R> {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> R;
}

impl Translate<Expr> for AstExpr {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        match self {
            AstExpr::Bool(b, _) => Expr::ConstInt(if *b { 1 } else { 0 }),
            AstExpr::Float(f, _) => Expr::ConstFloat(*f),
            AstExpr::Int(i, _) => Expr::ConstInt(*i),
            AstExpr::Str(..) => todo!(),
            AstExpr::Binary(binary) => Expr::Binary(
                match binary.op.kind {
                    TokenKind::Plus => BinOp::Plus,
                    TokenKind::Minus => BinOp::Minus,
                    TokenKind::Star => BinOp::Mul,
                    TokenKind::Slash => BinOp::Div,
                    TokenKind::BangEqual => BinOp::Cmp(RelOp::NotEqual),
                    TokenKind::EqualEqual => BinOp::Cmp(RelOp::Equal),
                    TokenKind::Greater => BinOp::Cmp(RelOp::Greater),
                    TokenKind::GreaterEqual => BinOp::Cmp(RelOp::GreaterEqual),
                    TokenKind::Less => BinOp::Cmp(RelOp::Less),
                    TokenKind::LessEqual => BinOp::Cmp(RelOp::LessEqual),
                    _ => unreachable!("not a valid binary operator"),
                },
                Box::new(binary.left.translate(translator)),
                Box::new(binary.right.translate(translator)),
            ),
            AstExpr::Call(call) => {
                let name = match *call.left {
                    AstExpr::Ident(ref ident) => String::from(&ident.name),
                    AstExpr::Access(_) => todo!(),
                    _ => unreachable!(),
                };
                Expr::Call(
                    name,
                    call.args
                        .iter()
                        .map(|arg| arg.translate(translator))
                        .collect(),
                )
            }
            AstExpr::Ident(ident) => {
                let id = translator.function.unwrap();
                let frame = translator.functions.get(&id).unwrap();
                *frame.get(&ident.name.to_string())
            }
            AstExpr::Unary(unary) => match unary.op.kind {
                TokenKind::Minus => Expr::Binary(
                    BinOp::Minus,
                    Box::new(Expr::ConstInt(0)),
                    Box::new(unary.expr.translate(translator)),
                ),
                TokenKind::Bang => Expr::Binary(
                    BinOp::Xor,
                    Box::new(unary.expr.translate(translator)),
                    Box::new(Expr::ConstInt(1)),
                ),
                _ => unreachable!("not a valid unary operator"),
            },
            AstExpr::Access(_) => todo!(),
            AstExpr::Init(_) => todo!(),
        }
    }
}

impl Translate<Stmt> for AstStmt {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let registers = F::registers();
        match self {
            AstStmt::If(c) => {
                let condition = c.condition.translate(translator);
                let t = translator.label();
                let f = translator.label();
                let done = translator.label();
                let is: Vec<Stmt> = c.is.iter().map(|stmt| stmt.translate(translator)).collect();
                let otherwise: Vec<Stmt> = c
                    .otherwise
                    .iter()
                    .map(|stmt| stmt.translate(translator))
                    .collect();
                let is = Stmt::from(&is[..]);
                let otherwise = Stmt::from(&otherwise[..]);
                Stmt::Seq {
                    left: Box::new(Stmt::Seq {
                        left: Box::new(Stmt::Seq {
                            left: Box::new(Stmt::Seq {
                                left: Box::new(Stmt::CJump {
                                    op: BinOp::Cmp(RelOp::Equal),
                                    left: Box::new(condition),
                                    right: Box::new(Expr::ConstInt(1)),
                                    t: t.clone(),
                                    f: f.clone(),
                                }),
                                right: Some(Box::new(Stmt::Seq {
                                    left: Box::new(Stmt::Label(t.clone())),
                                    right: Some(Box::new(is)),
                                })),
                            }),
                            right: Some(Box::new(Stmt::Jump(done.clone()))),
                        }),
                        right: Some(Box::new(Stmt::Seq {
                            left: Box::new(Stmt::Label(f)),
                            right: Some(Box::new(otherwise)),
                        })),
                    }),
                    right: Some(Box::new(Stmt::Label(done))),
                }
            }
            AstStmt::While(c) => {
                let condition = c.condition.translate(translator);
                let t = translator.label();
                let f = translator.label();
                let test = translator.label();
                let mut body: Vec<Stmt> = c
                    .body
                    .iter()
                    .map(|stmt| stmt.translate(translator))
                    .collect();
                body.push(Stmt::Jump(test.clone()));
                let body = Stmt::from(&body[..]);
                Stmt::Seq {
                    left: Box::new(Stmt::Seq {
                        left: Box::new(Stmt::Seq {
                            left: Box::new(Stmt::Label(test)),
                            right: Some(Box::new(Stmt::CJump {
                                op: BinOp::Cmp(RelOp::Equal),
                                left: Box::new(condition),
                                right: Box::new(Expr::ConstInt(1)),
                                t: t.clone(),
                                f: f.clone(),
                            })),
                        }),
                        right: Some(Box::new(Stmt::Seq {
                            left: Box::new(Stmt::Label(t)),
                            right: Some(Box::new(body)),
                        })),
                    }),
                    right: Some(Box::new(Stmt::Label(f))),
                }
            }
            AstStmt::Assign(assign) => {
                let target = match assign.target {
                    AstExpr::Ident(ref ident) => {
                        let id = translator.function.unwrap();
                        let frame = translator.functions.get(&id).unwrap();
                        frame.get(&ident.name.to_string())
                    }
                    AstExpr::Access(_) => todo!(),
                    _ => unreachable!(),
                };
                let expr = Box::new(assign.expr.translate(translator));
                Stmt::MoveMem { target, expr }
            }
            AstStmt::Expr(e) => Stmt::Expr(Box::new(e.translate(translator))),
            AstStmt::Return(ret) => Stmt::MoveTemp {
                target: Box::new(Expr::Temp(registers.ret.value.to_string())),
                expr: Box::new(ret.expr.translate(translator)),
            },
            AstStmt::Var(var) => {
                let id = translator.function.unwrap();
                let name = var.name.to_string();
                let frame = translator.functions.get_mut(&id).unwrap();
                let target = frame.allocate(&name);
                let expr = Box::new(var.expr.translate(translator));
                Stmt::MoveMem { target, expr }
            }
        }
    }
}

impl Translate<Stmt> for AstDecl {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        match self {
            AstDecl::Function(function) => {
                // Allocate a new frame for the function
                let frame = F::new(function);
                translator.functions.insert(function.id, frame);
                translator.function = Some(function.id);
                // Translate the body of the function
                let stmts: Vec<Stmt> = function
                    .body
                    .iter()
                    .map(|stmt| stmt.translate(translator))
                    .collect();
                Stmt::from(&stmts[..])
            }
            AstDecl::Record(_) => todo!(),
            AstDecl::Constant(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{ast, kyir::Translator, Source};

    use super::arch::amd64::Amd64;

    #[test]
    fn exp() -> Result<(), Box<dyn Error>> {
        let ast = ast::Ast::from_source(&Source::new("test-cases/conditions.kya")?)?;
        let mut translator: Translator<Amd64> = Translator::new();
        let res = translator.translate(&ast.nodes);
        dbg!(&res);
        dbg!(&translator.functions);

        Ok(())
    }
}

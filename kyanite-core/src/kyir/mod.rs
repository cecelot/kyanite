use std::collections::HashMap;

use crate::{
    ast::Expr as AstExpr,
    ast::{node::RecordDecl, Decl as AstDecl, Initializer, Stmt as AstStmt, Type},
    pass::{AccessMap, SymbolTable},
    token::{Token, TokenKind},
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
    ESeq { stmt: Box<Stmt>, expr: Box<Expr> },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Moves `expr` into the temporary specified by `target` (an [`Expr::Temp`])
    MoveTemp {
        target: Box<Expr>,
        expr: Box<Expr>,
    },
    /// Moves `expr` into the location in the stack frame specified by `target` (an [`Expr::Binary`])
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

trait Flatten<R, A> {
    fn flatten(&self, aux: A) -> R;
}

impl<F: Frame> Flatten<Vec<Expr>, &'_ mut Translator<'_, F>> for Vec<Initializer> {
    fn flatten(&self, translator: &'_ mut Translator<'_, F>) -> Vec<Expr> {
        self.iter()
            .flat_map(|init| match &init.expr {
                AstExpr::Init(nested) => nested.initializers.flatten(translator),
                _ => vec![init.expr.translate(translator)],
            })
            .collect()
    }
}

impl Flatten<Vec<(String, Type)>, &'_ SymbolTable> for RecordDecl {
    fn flatten(&self, symbols: &'_ SymbolTable) -> Vec<(String, Type)> {
        self.fields
            .iter()
            .flat_map(|field| match Type::from(&field.ty) {
                Type::UserDefined(name) => symbols
                    .get(&Token::from(name))
                    .unwrap()
                    .record()
                    .flatten(symbols),
                _ => vec![(field.name.to_string(), Type::from(&field.ty))],
            })
            .collect()
    }
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

pub struct Translator<'a, F: Frame> {
    functions: HashMap<usize, F>,
    function: Option<usize>,
    accesses: &'a AccessMap,
    symbols: &'a SymbolTable,
    labels: usize,
    names: usize,
}

impl<'a, F: Frame> Translator<'a, F> {
    #[allow(dead_code)]
    pub fn new(accesses: &'a AccessMap, symbols: &'a SymbolTable) -> Self {
        Self {
            functions: HashMap::new(),
            function: None,
            accesses,
            symbols,
            labels: 0,
            names: 0,
        }
    }

    #[allow(dead_code)]
    pub fn translate(&mut self, ast: &[AstDecl]) -> Vec<Stmt> {
        ast.iter().map(|decl| decl.translate(self)).collect()
    }

    fn frame(&self) -> &F {
        let id: usize = self.function.unwrap();
        self.functions.get(&id).unwrap()
    }

    fn label(&mut self) -> String {
        let label = format!("L{}", self.labels);
        self.labels += 1;
        label
    }

    fn name(&mut self) -> String {
        let name = format!("N{}", self.names);
        self.names += 1;
        name
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
            AstExpr::Ident(ident) => *translator.frame().get(&ident.name.to_string(), None, None),
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
            AstExpr::Access(access) => {
                let temp = translator.name();
                let frame = translator.frame();
                let (symbols, _, _) = translator.accesses.get(&access.id).unwrap();
                let rec = symbols.first().unwrap().record();
                let flat = rec.flatten(translator.symbols);
                let parent = access.chain.first().unwrap().ident().name.to_string();
                let last = access.chain.last().unwrap().ident().name.to_string();
                let (index, _) = flat
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == &last)
                    .unwrap();
                *frame.get(&parent, Some(temp), Some(index))
            }
            AstExpr::Init(init) => {
                let registers = F::registers();
                let ty = Type::from(&init.name);
                let initializers = init.initializers.flatten(translator);
                let name = translator.name();
                let id = translator.function.unwrap();
                let frame = translator.functions.get_mut(&id).unwrap();
                frame.allocate(translator.symbols, &name, Some(&ty));
                let begin = frame.get_offset(&name);
                let end = begin - i64::try_from((initializers.len() - 1) * 8).unwrap();
                let stmts: Vec<Stmt> = initializers
                    .into_iter()
                    .enumerate()
                    .map(|(index, expr)| Stmt::MoveMem {
                        target: Box::new(Expr::Binary(
                            BinOp::Plus,
                            Box::new(Expr::Temp(registers.frame.to_string())),
                            Box::new(Expr::ConstInt(end + i64::try_from(index * 8).unwrap())),
                        )),
                        expr: Box::new(expr),
                    })
                    .collect();
                // Evaluate the initializers, then return start address of initialized memory for record
                Expr::ESeq {
                    stmt: Box::new(Stmt::from(&stmts[..])),
                    expr: Box::new(Expr::ConstInt(begin)),
                }
            }
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
                let target = Box::new(assign.target.translate(translator));
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
                // No matter what, variables are always F::word_size() (either pointer to first element or the value itself)
                let target = frame.allocate(translator.symbols, &name, None);
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
            AstDecl::Record(_) => Stmt::Noop,
            AstDecl::Constant(_) => todo!(),
        }
    }
}

macro_rules! assert_ir {
    ($($path:expr => $name:ident),*) => {
        #[cfg(test)]
        mod tests {
            use std::collections::HashMap;

            use crate::{
                ast,
                kyir::Translator,
                pass::{SymbolTable, TypeCheckPass},
                PipelineError, Source,
            };

            use super::arch::amd64::Amd64;

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let source = Source::new($path)?;
                    let ast = ast::Ast::from_source(&source)?;
                    let symbols = SymbolTable::from(&ast.nodes);
                    let mut accesses = HashMap::new();
                    let mut pass = TypeCheckPass::new(&symbols, &mut accesses, source, &ast.nodes);
                    pass.run().map_err(PipelineError::TypeError)?;
                    let mut translator: Translator<Amd64> = Translator::new(&accesses, &symbols);
                    let res = translator.translate(&ast.nodes);
                    insta::with_settings!({snapshot_path => "../../snapshots"}, {
                        insta::assert_debug_snapshot!(&res);
                    });

                    Ok(())
                }
            )*
        }
    };
}

assert_ir!(
    "test-cases/kyir/varied.kya" => varied
);

mod canon;

pub use canon::canonicalize;

use crate::{
    ast::Expr as AstExpr,
    ast::{
        node::{
            Access, Assign, Binary, Call, FuncDecl, Ident, If, Init, RecordDecl, Return, Unary,
            VarDecl, While,
        },
        Decl as AstDecl, Initializer, Stmt as AstStmt, Type,
    },
    backend::kyir::{arch::Frame, Label, Temp},
    pass::{AccessMap, SymbolTable},
    token::Kind,
};
use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

pub struct Translator<'a, F: Frame> {
    functions: HashMap<usize, F>,
    function: Option<usize>,
    accesses: &'a AccessMap,
    symbols: &'a SymbolTable,
}

impl<'a, F: Frame> Translator<'a, F> {
    pub fn new(accesses: &'a AccessMap, symbols: &'a SymbolTable) -> Self {
        Self {
            functions: HashMap::new(),
            function: None,
            accesses,
            symbols,
        }
    }

    pub fn translate(&mut self, ast: &[AstDecl]) -> Vec<Stmt> {
        ast.iter().map(|decl| decl.translate(self)).collect()
    }

    fn frame(&self) -> &F {
        let id: usize = self.function.unwrap();
        self.functions.get(&id).unwrap()
    }

    pub fn functions(self) -> HashMap<usize, F> {
        self.functions
    }
}

trait Translate<R> {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> R;
}

impl Translate<Expr> for AstExpr {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        match self {
            AstExpr::Bool(b, _) => Expr::ConstInt((*b).into()),
            AstExpr::Float(f, _) => Expr::ConstFloat(*f),
            AstExpr::Int(i, _) => Expr::ConstInt(*i),
            AstExpr::Str(..) => todo!(),
            AstExpr::Binary(binary) => binary.translate(translator),
            AstExpr::Call(call) => call.translate(translator),
            AstExpr::Ident(ident) => ident.translate(translator),
            AstExpr::Unary(unary) => unary.translate(translator),
            AstExpr::Access(access) => access.translate(translator),
            AstExpr::Init(init) => init.translate(translator),
        }
    }
}

impl Translate<Stmt> for AstStmt {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        match self {
            AstStmt::If(c) => c.translate(translator),
            AstStmt::While(c) => c.translate(translator),
            AstStmt::Assign(assign) => assign.translate(translator),
            AstStmt::Expr(e) => e.translate(translator),
            AstStmt::Return(ret) => ret.translate(translator),
            AstStmt::Var(var) => var.translate(translator),
        }
    }
}

impl Translate<Stmt> for AstDecl {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        match self {
            AstDecl::Function(function) => function.translate(translator),
            AstDecl::Record(rec) => rec.translate(translator),
            AstDecl::Constant(_) => todo!(),
        }
    }
}

impl Translate<Expr> for Binary {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        Expr::checked_binary(
            self.op.kind.into(),
            Box::new(self.left.translate(translator)),
            self.right.translate(translator),
        )
    }
}

impl Translate<Expr> for Call {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        let args: Vec<_> = self
            .args
            .iter()
            .map(|arg| arg.translate(translator))
            .collect();
        let name = match *self.left {
                    AstExpr::Ident(ref ident) => ident.name.to_string(),
                    AstExpr::Access(_) => todo!(),
                    _ => panic!("Expected either `AstExpr::Ident` or `AstExpr::Access` on left side of call expression"),
                };
        Expr::Call { name, args }
    }
}

impl Translate<Expr> for Ident {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        translator.frame().get(&self.name.to_string(), None, None)
    }
}

impl Translate<Expr> for Unary {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        match self.op.kind {
            Kind::Minus => Expr::Binary {
                op: BinOp::Minus,
                left: Box::new(Expr::ConstInt(0)),
                right: Box::new(self.expr.translate(translator)),
            },
            Kind::Bang => Expr::Binary {
                op: BinOp::Xor,
                left: Box::new(self.expr.translate(translator)),
                right: Box::new(Expr::ConstInt(1)),
            },
            _ => unreachable!("not a valid unary operator"),
        }
    }
}

impl Translate<Expr> for Access {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        let temp = Temp::next();
        let frame = translator.frame();
        let aux = translator.accesses.get(&self.id).unwrap();
        let rec = aux.symbols.first().unwrap().record();
        let flat = rec.flatten(translator.symbols);
        let parent = self.chain.first().unwrap().ident().name.to_string();
        let last = self.chain.last().unwrap().ident().name.to_string();
        let (index, _) = flat
            .iter()
            .enumerate()
            .find(|(_, (name, _))| name == &last)
            .unwrap();
        frame.get(&parent, Some(temp), Some(index))
    }
}

impl Translate<Expr> for Init {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        let registers = F::registers();
        let ty = Type::from(&self.name);
        let initializers = self.initializers.flatten(translator);
        let name = Temp::next();
        let id = translator.function.unwrap();
        let frame = translator.functions.get_mut(&id).unwrap();
        frame.allocate(translator.symbols, &name, Some(&ty));
        let begin = frame.get_offset(&name);
        let end = begin - i64::try_from((initializers.len() - 1) * F::word_size()).unwrap();
        let stmts: Vec<Stmt> = initializers
            .into_iter()
            .enumerate()
            .map(|(index, expr)| {
                Stmt::checked_move(
                    Box::new(Expr::Binary {
                        op: BinOp::Plus,
                        left: Box::new(Expr::Temp(registers.frame.to_string())),
                        right: Box::new(Expr::ConstInt(
                            end + i64::try_from(index * F::word_size()).unwrap(),
                        )),
                    }),
                    expr,
                )
            })
            .collect();
        // Evaluate the initializers, then return start address of initialized memory for record
        Expr::ESeq {
            stmt: Box::new(Stmt::from(&stmts[..])),
            expr: Box::new(Expr::ConstInt(begin)),
            id,
        }
    }
}

impl Translate<Stmt> for If {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let condition = match &self.condition {
            AstExpr::Int(i, _) => Expr::Binary {
                op: BinOp::Cmp(RelOp::Equal),
                left: Box::new(Expr::ConstInt(*i)),
                right: Box::new(Expr::ConstInt(0)),
            },
            c => c.translate(translator),
        };
        let t = Label::next();
        let f = Label::next();
        let done = Label::next();
        let is: Vec<Stmt> = self
            .is
            .iter()
            .map(|stmt| stmt.translate(translator))
            .collect();
        let otherwise: Vec<Stmt> = self
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
                            op: BinOp::Cmp(condition.relation().unwrap()),
                            condition: Box::new(condition),
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
}

impl Translate<Stmt> for While {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let condition = match &self.condition {
            AstExpr::Int(i, _) => Expr::Binary {
                op: BinOp::Cmp(RelOp::Equal),
                left: Box::new(Expr::ConstInt(*i)),
                right: Box::new(Expr::ConstInt(0)),
            },
            c => c.translate(translator),
        };
        let t = Label::next();
        let f = Label::next();
        let test = Label::next();
        let mut body: Vec<Stmt> = self
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
                        op: BinOp::Cmp(condition.relation().unwrap()),
                        condition: Box::new(condition),
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
}

impl Translate<Stmt> for Assign {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        Stmt::checked_move(
            Box::new(self.target.translate(translator)),
            self.expr.translate(translator),
        )
    }
}

impl Translate<Stmt> for AstExpr {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        Stmt::Expr(Box::new(self.translate(translator)))
    }
}

impl Translate<Stmt> for Return {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let registers = F::registers();
        Stmt::checked_move(
            Box::new(Expr::Temp(registers.ret.value.to_string())),
            self.expr.translate(translator),
        )
    }
}

impl Translate<Stmt> for VarDecl {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let id = translator.function.unwrap();
        let name = self.name.to_string();
        let frame = translator.functions.get_mut(&id).unwrap();
        // No matter what, variables are always F::word_size() (either pointer to first element or the value itself)
        let target = frame.allocate(translator.symbols, &name, None);
        let expr = self.expr.translate(translator);
        Stmt::checked_move(Box::new(target), expr)
    }
}

impl Translate<Stmt> for FuncDecl {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let frame = F::new(self);
        translator.functions.insert(self.id, frame);
        translator.function = Some(self.id);
        let stmts: Vec<Stmt> = vec![Stmt::Label(self.name.to_string())]
            .into_iter()
            .chain(self.body.iter().map(|stmt| stmt.translate(translator)))
            .collect();
        Stmt::from(&stmts[..])
    }
}

impl Translate<Stmt> for RecordDecl {
    fn translate<F: Frame>(&self, _: &mut Translator<F>) -> Stmt {
        Stmt::Noop
    }
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
            .flat_map(|field| {
                let ty = Type::from(&field.ty);
                match ty {
                    Type::UserDefined(name) => symbols
                        .get(&name.to_string())
                        .unwrap()
                        .record()
                        .flatten(symbols),
                    _ => vec![(field.name.to_string(), ty)],
                }
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    ConstInt(i64),
    ConstFloat(f64),
    Temp(String),
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Mem(Box<Expr>),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    ESeq {
        stmt: Box<Stmt>,
        expr: Box<Expr>,
        id: usize,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Moves `expr` into the location specified by `target` (either a temporary or a memory offset in the frame)
    Move {
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
        condition: Box<Expr>,
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

/// Returns a new `Expr` that loads the value at the memory location specified by `expr`
/// and stores it in a temporary. This is required because (a) moves from memory address to memory
/// address are unsupported and (b) binary operations require at least one of their operands to be
/// in registers.
fn wrap_memory_load(expr: Box<Expr>) -> Expr {
    let temp = Temp::next();
    Expr::eseq(
        Box::new(Stmt::Move {
            target: Box::new(Expr::Temp(temp.clone())),
            expr,
        }),
        Box::new(Expr::Temp(temp)),
    )
}

impl Expr {
    /// Returns a new `Expr` where `left` and `right` are not both memory addresses.
    fn checked_binary(op: BinOp, left: Box<Expr>, right: Expr) -> Self {
        let right = match (&*left, right) {
            (Expr::Mem(_), Expr::Mem(expr)) => wrap_memory_load(expr),
            (Expr::Mem(_), Expr::ESeq { stmt, expr, id }) => {
                if matches!(*expr, Expr::Mem(_)) {
                    wrap_memory_load(expr)
                } else {
                    Expr::ESeq { stmt, expr, id }
                }
            }
            (_, right) => right,
        };
        Self::Binary {
            op,
            left,
            right: Box::new(right),
        }
    }
}

impl Stmt {
    /// Returns a new `Stmt` where `target` and `expr` are not both memory addresses.
    fn checked_move(target: Box<Expr>, expr: Expr) -> Self {
        let expr = match (&*target, expr) {
            (Expr::Mem(_), Expr::Mem(expr)) => wrap_memory_load(expr),
            (Expr::Mem(_), Expr::ESeq { stmt, expr, id }) => {
                if matches!(*expr, Expr::Mem(_)) {
                    wrap_memory_load(expr)
                } else {
                    Expr::ESeq { stmt, expr, id }
                }
            }
            (_, expr) => expr,
        };
        Self::Move {
            target,
            expr: Box::new(expr),
        }
    }
}

impl Expr {
    pub fn eseq(stmt: Box<Stmt>, expr: Box<Expr>) -> Self {
        static ID: AtomicUsize = AtomicUsize::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        Self::ESeq { stmt, expr, id }
    }

    pub fn relation(&self) -> Option<RelOp> {
        match self {
            Self::Binary {
                op: BinOp::Cmp(rel),
                ..
            } => Some(*rel),
            Self::ConstInt(_) => Some(RelOp::Equal),
            _ => None,
        }
    }

    pub fn temp(&self) -> Option<String> {
        match self {
            Self::Temp(t) => Some(t.clone()),
            _ => None,
        }
    }

    pub fn int(&self) -> Option<i64> {
        match self {
            Self::ConstInt(i) => Some(*i),
            _ => None,
        }
    }

    pub fn binary(self) -> Option<(BinOp, Box<Self>, Box<Self>)> {
        match self {
            Self::Binary { op, left, right } => Some((op, left, right)),
            _ => None,
        }
    }
}

impl Stmt {
    pub fn label(&self) -> String {
        match self {
            Self::Seq { left, .. } => left.label(),
            Self::Label(label) => label.clone(),
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

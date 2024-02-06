mod canon;

pub use canon::canonicalize;

#[allow(clippy::wildcard_imports)]
use crate::{
    ast::Expr as AstExpr,
    ast::{self, Decl as AstDecl, Stmt as AstStmt},
    backend::kyir::{arch::Frame, ir::*},
    pass::{AccessMap, SymbolTable},
    token::{Kind, Span, Token},
};
use std::collections::HashMap;

pub struct Translator<'a, F: Frame> {
    functions: HashMap<usize, F>,
    function: Option<usize>,
    accesses: &'a AccessMap,
    symbols: &'a SymbolTable,
    ctx: Context,
}

struct Context {
    ret: bool,
    name: Vec<String>,
}

impl<'a, F: Frame> Translator<'a, F> {
    pub fn new(accesses: &'a AccessMap, symbols: &'a SymbolTable) -> Self {
        Self {
            functions: HashMap::new(),
            function: None,
            ctx: Context {
                ret: false,
                name: vec![],
            },
            accesses,
            symbols,
        }
    }

    #[must_use]
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
            AstExpr::Int(i) => i.translate(translator),
            AstExpr::Float(f) => f.translate(translator),
            AstExpr::Bool(b) => b.translate(translator),
            AstExpr::Range(_r) => todo!(),
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
            AstStmt::While(w) => w.translate(translator),
            AstStmt::For(f) => f.translate(translator),
            AstStmt::Assign(a) => a.translate(translator),
            AstStmt::Expr(e) => e.translate(translator),
            AstStmt::Return(r) => r.translate(translator),
            AstStmt::Var(v) => v.translate(translator),
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

impl Translate<Expr> for ast::node::Literal<i64> {
    fn translate<F: Frame>(&self, _: &mut Translator<F>) -> Expr {
        Const::<i64>::int(self.value)
    }
}

impl Translate<Expr> for ast::node::Literal<f64> {
    fn translate<F: Frame>(&self, _: &mut Translator<F>) -> Expr {
        Const::<f64>::float(self.value)
    }
}

impl Translate<Expr> for ast::node::Literal<bool> {
    fn translate<F: Frame>(&self, _: &mut Translator<F>) -> Expr {
        Const::<i64>::int(self.value.into())
    }
}

impl Translate<Expr> for ast::node::Binary {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        Expr::checked_binary(
            self.op.kind.into(),
            self.left.translate(translator),
            self.right.translate(translator),
        )
    }
}

impl Translate<Expr> for ast::node::Call {
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
        let temp = Temp::next();
        let id = translator.function.unwrap();
        let frame = translator.functions.get_mut(&id).unwrap();
        let saved = frame.allocate(translator.symbols, &temp, None);
        ESeq::wrapped(
            Seq::wrapped(
                Stmt::Expr(Box::new(Call::wrapped(name, args))),
                Some(Move::wrapped(
                    saved.clone(),
                    Temp::wrapped(F::registers().ret.value.into()),
                )),
            ),
            saved,
        )
    }
}

impl Translate<Expr> for ast::node::Ident {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        translator.frame().get(&self.name.to_string())
    }
}

impl Translate<Expr> for ast::node::Unary {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        match self.op.kind {
            Kind::Minus => Binary::wrapped(
                BinOp::Minus,
                Const::<i64>::int(0),
                self.expr.translate(translator),
            ),
            Kind::Bang => Binary::wrapped(
                BinOp::Xor,
                self.expr.translate(translator),
                Const::<i64>::int(1),
            ),
            _ => unreachable!("not a valid unary operator"),
        }
    }
}

impl Translate<Expr> for ast::node::Access {
    // heh, this is basically the spiritual equivalent of LLVM's getelementptr
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        let frame = translator.frame();
        let meta = translator.accesses.get(&self.id).unwrap();
        let ident = self.chain.first().unwrap().ident().name.to_string();
        let base = frame.get(&ident);
        let temp = Temp::next();
        let initial = [Stmt::checked_move(Temp::wrapped(temp.clone()), base)];
        let stmts: Vec<_> = initial
            .into_iter()
            .chain(meta.indices.iter().flat_map(|&field| {
                let offset: i64 = (field * F::word_size()).try_into().unwrap();
                vec![
                    Stmt::Expr(Box::new(Binary::wrapped(
                        BinOp::Plus,
                        Temp::wrapped(temp.clone()),
                        Const::<i64>::int(offset),
                    ))),
                    Stmt::checked_move(
                        Temp::wrapped(temp.clone()),
                        Temp::dereferenced(temp.clone()),
                    ),
                ]
            }))
            .collect();
        ESeq::wrapped(Stmt::from(&stmts[..]), Temp::wrapped(temp))
    }
}

impl Translate<Expr> for ast::node::Init {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        let registers = F::registers();
        let id = translator.function.unwrap();
        let frame = translator.functions.get_mut(&id).unwrap();
        let temp = Temp::next();
        let name = translator.ctx.name.join(".");
        let base = frame.allocate(translator.symbols, &name, None);
        let setup = [
            Stmt::Expr(Box::new(Call::wrapped(
                "alloc".into(),
                vec![Const::<i64>::int(
                    (self.initializers.len() * F::word_size())
                        .try_into()
                        .unwrap(),
                )],
            ))),
            Stmt::checked_move(base.clone(), Temp::wrapped(registers.ret.value.to_string())),
        ];
        let stmts: Vec<_> = setup
            .into_iter()
            .chain(self.initializers.iter().enumerate().flat_map(|(i, init)| {
                translator.ctx.name.push(init.name.to_string());
                let value = match &init.expr {
                    AstExpr::Init(init) => init.translate(translator),
                    _ => init.expr.translate(translator),
                };
                let offset: i64 = (i * F::word_size()).try_into().unwrap();
                translator.ctx.name.pop();
                vec![
                    Stmt::checked_move(Temp::wrapped(temp.clone()), base.clone()), // copy base pointer
                    Stmt::Expr(Box::new(Binary::wrapped(
                        BinOp::Plus,
                        Temp::wrapped(temp.clone()),
                        Const::<i64>::int(offset),
                    ))), // add offset to base pointer
                    Stmt::checked_move(Temp::dereferenced(temp.clone()), value), // store value at offset
                ]
            }))
            .collect();
        ESeq::wrapped(Stmt::from(&stmts[..]), base)
    }
}

impl Translate<Stmt> for ast::node::If {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let condition = match &self.condition {
            AstExpr::Int(i) => Binary::wrapped(
                BinOp::Cmp(RelOp::Equal),
                Const::<i64>::int(i.value),
                Const::<i64>::int(0),
            ),
            c => c.translate(translator),
        };
        let is: Vec<Stmt> = self
            .is
            .iter()
            .map(|stmt| stmt.translate(translator))
            .collect();
        let is = Stmt::from(&is[..]);
        let done = if translator.ctx.ret {
            translator.ctx.ret = false;
            let id = translator.function.unwrap();
            let frame = translator.functions.get_mut(&id).unwrap();
            format!("{}.epilogue", frame.label())
        } else {
            Label::next()
        };
        let otherwise: Vec<Stmt> = self
            .otherwise
            .iter()
            .map(|stmt| stmt.translate(translator))
            .collect();
        let otherwise = Stmt::from(&otherwise[..]);
        let t = Label::next();
        let f = Label::next();
        Seq::wrapped(
            Seq::wrapped(
                Seq::wrapped(
                    Seq::wrapped(
                        CJump::wrapped(
                            BinOp::Cmp(condition.relation().unwrap()),
                            condition,
                            t.clone(),
                            f.clone(),
                        ),
                        Some(Seq::wrapped(Label::wrapped(t), Some(is))),
                    ),
                    Some(Jump::wrapped(done.clone())),
                ),
                Some(Seq::wrapped(Label::wrapped(f), Some(otherwise))),
            ),
            (!done.ends_with("epilogue")).then_some(Label::wrapped(done)),
        )
    }
}

impl Translate<Stmt> for ast::node::While {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let condition = match &self.condition {
            AstExpr::Int(i) => Binary::wrapped(
                BinOp::Cmp(RelOp::Equal),
                Const::<i64>::int(i.value),
                Const::<i64>::int(0),
            ),
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
        body.push(Jump::wrapped(test.clone()));
        let body = Stmt::from(&body[..]);
        Seq::wrapped(
            Seq::wrapped(
                Seq::wrapped(
                    Label::wrapped(test),
                    Some(CJump::wrapped(
                        BinOp::Cmp(condition.relation().unwrap()),
                        condition,
                        t.clone(),
                        f.clone(),
                    )),
                ),
                Some(Seq::wrapped(Label::wrapped(t.clone()), Some(body))),
            ),
            Some(Label::wrapped(f)),
        )
    }
}

impl Translate<Stmt> for ast::node::For {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let range = self.iter.range();
        let cur = ast::node::Ident::wrapped(self.index.clone());
        let start = ast::node::VarDecl::wrapped(
            cur.clone().ident().name.clone(),
            Token::new(Kind::Literal, None, Span::default()),
            range.start.clone(),
        );
        let w = ast::node::While {
            condition: ast::node::Binary::wrapped(
                cur.clone(),
                Token::new(Kind::LessEqual, None, Span::default()),
                range.end.clone(),
            ),
            body: self
                .body
                .clone()
                .into_iter()
                .chain(vec![ast::node::Assign::wrapped(
                    cur.clone(),
                    ast::node::Binary::wrapped(
                        cur,
                        Token::new(Kind::Plus, None, Span::default()),
                        ast::node::Literal::<i64>::int(
                            1,
                            Token::new(Kind::Literal, Some("1"), Span::default()),
                        ),
                    ),
                )])
                .collect(),
        };
        let stmts: Vec<Stmt> = vec![start.translate(translator), w.translate(translator)];
        Stmt::from(&stmts[..])
    }
}

impl Translate<Stmt> for ast::node::Assign {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        Stmt::checked_move(
            self.target.translate(translator),
            self.expr.translate(translator),
        )
    }
}

impl Translate<Stmt> for AstExpr {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        Stmt::Expr(Box::new(self.translate(translator)))
    }
}

impl Translate<Stmt> for ast::node::Return {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let registers = F::registers();
        translator.ctx.ret = true;
        Stmt::checked_move(
            Temp::wrapped(registers.ret.value.to_string()),
            self.expr.translate(translator),
        )
    }
}

impl Translate<Stmt> for ast::node::VarDecl {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let name = self.name.to_string();
        if matches!(self.expr, AstExpr::Init(_)) {
            translator.ctx.name.push(name.clone());
        }
        let expr = self.expr.translate(translator);
        translator.ctx.name.clear();
        let id = translator.function.unwrap();
        let frame = translator.functions.get_mut(&id).unwrap();
        // No matter what, variables are always F::word_size() (either pointer to first element or the value itself)
        let target = frame.allocate(translator.symbols, &name, None);
        Stmt::checked_move(target, expr)
    }
}

impl Translate<Stmt> for ast::node::FuncDecl {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let frame = F::new(self);
        translator.functions.insert(self.id, frame);
        translator.function = Some(self.id);
        let stmts: Vec<Stmt> = vec![Label::wrapped(self.name.to_string())]
            .into_iter()
            .chain(self.body.iter().map(|stmt| stmt.translate(translator)))
            .collect();
        Stmt::from(&stmts[..])
    }
}

impl Translate<Stmt> for ast::node::RecordDecl {
    fn translate<F: Frame>(&self, _: &mut Translator<F>) -> Stmt {
        Stmt::Noop
    }
}

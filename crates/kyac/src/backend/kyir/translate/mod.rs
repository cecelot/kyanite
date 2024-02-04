mod canon;

pub use canon::canonicalize;

#[allow(clippy::wildcard_imports)]
use crate::{
    ast::Expr as AstExpr,
    ast::{self, Decl as AstDecl, Stmt as AstStmt, Type},
    backend::kyir::{arch::Frame, ir::*},
    pass::{AccessMap, SymbolTable},
    token::Kind,
};
use std::collections::HashMap;

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
        Call::wrapped(name, args)
    }
}

impl Translate<Expr> for ast::node::Ident {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        translator.frame().get(&self.name.to_string(), None, None)
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
            .expect("field access for non-terminal fields is not yet supported");
        let base = frame.get(&parent, None, None);
        let offset: i64 = (index * F::word_size()).try_into().unwrap();
        let stmts = [
            Stmt::checked_move(Temp::wrapped(temp.clone()), base), // copy base pointer
            Stmt::Expr(Box::new(Binary::wrapped(
                BinOp::Plus,
                Temp::wrapped(temp.clone()),
                Const::<i64>::int(offset),
            ))), // add offset to base pointer
            Stmt::checked_move(
                Temp::wrapped(temp.clone()),
                Temp::dereferenced(temp.clone()),
            ),
        ];
        ESeq::wrapped(Stmt::from(&stmts[..]), Temp::wrapped(temp))
    }
}

impl Translate<Expr> for ast::node::Init {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Expr {
        let registers = F::registers();
        let initializers = self.initializers.flatten(translator);
        let id = translator.function.unwrap();
        let frame = translator.functions.get_mut(&id).unwrap();
        let temp = Temp::next();
        // `temp` is guaranteed to be unique
        let base = frame.allocate(translator.symbols, &format!("_alloc_{temp}"), None);
        let setup = [
            Stmt::Expr(Box::new(Call::wrapped(
                "alloc".into(),
                vec![Const::<i64>::int(
                    (initializers.len() * F::word_size()).try_into().unwrap(),
                )],
            ))),
            Stmt::checked_move(base.clone(), Temp::wrapped(registers.ret.value.to_string())),
        ];
        let stmts: Vec<_> = setup
            .into_iter()
            .chain(initializers.into_iter().enumerate().flat_map(|(i, init)| {
                let offset: i64 = (i * F::word_size()).try_into().unwrap();
                vec![
                    Stmt::checked_move(Temp::wrapped(temp.clone()), base.clone()), // copy base pointer
                    Stmt::Expr(Box::new(Binary::wrapped(
                        BinOp::Plus,
                        Temp::wrapped(temp.clone()),
                        Const::<i64>::int(offset),
                    ))), // add offset to base pointer
                    Stmt::checked_move(Temp::dereferenced(temp.clone()), init), // store value at offset
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
            Some(Label::wrapped(done)),
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
        Stmt::checked_move(
            Temp::wrapped(registers.ret.value.to_string()),
            self.expr.translate(translator),
        )
    }
}

impl Translate<Stmt> for ast::node::VarDecl {
    fn translate<F: Frame>(&self, translator: &mut Translator<F>) -> Stmt {
        let id = translator.function.unwrap();
        let name = self.name.to_string();
        let frame = translator.functions.get_mut(&id).unwrap();
        // No matter what, variables are always F::word_size() (either pointer to first element or the value itself)
        let target = frame.allocate(translator.symbols, &name, None);
        let expr = self.expr.translate(translator);
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

trait Flatten<R, A> {
    fn flatten(&self, aux: A) -> R;
}

impl<F: Frame> Flatten<Vec<Expr>, &'_ mut Translator<'_, F>> for Vec<ast::node::Initializer> {
    fn flatten(&self, translator: &'_ mut Translator<'_, F>) -> Vec<Expr> {
        self.iter()
            .flat_map(|init| match &init.expr {
                AstExpr::Init(nested) => nested.initializers.flatten(translator),
                _ => vec![init.expr.translate(translator)],
            })
            .collect()
    }
}

impl Flatten<Vec<(String, Type)>, &'_ SymbolTable> for ast::node::RecordDecl {
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

mod canon;

pub(super) use canon::canonicalize;

#[allow(clippy::wildcard_imports)]
use crate::{
    ast::{self, node::FuncDecl, ty::Type, Decl as AstDecl, Expr as AstExpr, Stmt as AstStmt},
    backend::kyir::{
        arch::{ArchInstr, Frame},
        ir::*,
    },
    pass::{ResolvedMetaInfo, Symbol, SymbolTable},
    token::{Kind, Span, Token},
};
use std::{collections::HashMap, ops::Sub};

pub struct Translator<'a, I: ArchInstr, F: Frame<I>> {
    functions: HashMap<usize, F>,
    function: Option<usize>,
    meta: &'a ResolvedMetaInfo,
    symbols: &'a SymbolTable,
    ctx: Context,
    _isa: std::marker::PhantomData<I>,
}

struct Context {
    ret: bool,
    name: Vec<String>,
    constants: Constants,
    mem: Option<Mem>,
    stmts: Vec<Stmt>,
}

impl<'a, I: ArchInstr, F: Frame<I>> Translator<'a, I, F> {
    pub fn new(symbols: &'a SymbolTable, meta: &'a ResolvedMetaInfo) -> Self {
        Self {
            _isa: std::marker::PhantomData,
            functions: HashMap::new(),
            function: None,
            ctx: Context {
                ret: false,
                constants: Constants::new(),
                name: vec![],
                mem: None,
                stmts: vec![],
            },
            symbols,
            meta,
        }
    }

    #[must_use]
    pub fn translate(&mut self, ast: &[AstDecl]) -> Vec<Stmt> {
        ast.iter().flat_map(|decl| decl.translate(self)).collect()
    }

    fn frame(&self) -> &F {
        let id: usize = self.function.unwrap();
        self.functions.get(&id).unwrap()
    }

    pub fn functions(&self) -> &HashMap<usize, F> {
        &self.functions
    }

    pub fn constants(&self) -> &HashMap<String, Vec<String>> {
        &self.ctx.constants
    }
}

trait Translate<R> {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> R;
}

impl Translate<Expr> for AstExpr {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Expr {
        match self {
            AstExpr::Int(i) => i.translate(translator),
            AstExpr::Float(f) => f.translate(translator),
            AstExpr::Bool(b) => b.translate(translator),
            AstExpr::Range(_) => unimplemented!("ranges are not valid as standalone expressions"),
            AstExpr::Str(s) => s.translate(translator),
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
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
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

impl Translate<Vec<Stmt>> for AstDecl {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Vec<Stmt> {
        match self {
            AstDecl::Function(function) => vec![function.translate(translator)],
            AstDecl::Class(cls) => cls.translate(translator),
            AstDecl::Constant(_) => todo!(),
        }
    }
}

impl Translate<Expr> for ast::node::Literal<&str> {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Expr {
        Expr::ConstStr(
            translator
                .ctx
                .constants
                .add(vec![self.value.to_string().replace('"', "")]),
        )
    }
}

impl Translate<Expr> for ast::node::Literal<i64> {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, _: &mut Translator<I, F>) -> Expr {
        Const::<i64>::int(self.value)
    }
}

impl Translate<Expr> for ast::node::Literal<f64> {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, _: &mut Translator<I, F>) -> Expr {
        Const::<f64>::float(self.value)
    }
}

impl Translate<Expr> for ast::node::Literal<bool> {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, _: &mut Translator<I, F>) -> Expr {
        Const::<i64>::int(self.value.into())
    }
}

impl Translate<Expr> for ast::node::Binary {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Expr {
        Expr::checked_binary(
            self.op.kind.into(),
            self.left.translate(translator),
            self.right.translate(translator),
        )
    }
}

impl Translate<Expr> for ast::node::Call {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Expr {
        let r = F::registers();
        let args: Vec<_> = self
            .args
            .iter()
            .map(|arg| arg.translate(translator))
            .collect();
        let mut cls = None;
        let name = match *self.left {
            AstExpr::Ident(ref ident) => ident.name.to_string(),
            AstExpr::Access(ref access) => {
                let meta = translator.meta.access.get(&access.id).unwrap();
                let name = meta.symbols.iter().rev().take(2).rev().map(|item| match item {
                    Symbol::Function(fun) => fun.name.to_string(),
                    Symbol::Class(c) => {
                        cls = Some(c);
                        c.name.to_string()
                    }
                    _ => unimplemented!(),
                }).fold(String::new(), |acc, item| format!("{acc}{item}."));
                name.trim_end_matches('.').to_string()
            },
            _ => panic!("Expected either `AstExpr::Ident` or `AstExpr::Access` on left side of call expression"),
        };
        let ptr = match *self.left {
            AstExpr::Ident(_) => translator.symbols[&name].is_ptr(),
            AstExpr::Access(ref access) => {
                let meta = translator.meta.access.get(&access.id).unwrap();
                meta.symbols.last().unwrap().is_ptr()
            }
            _ => unimplemented!(),
        };
        let temp = Temp::next();
        let id = translator.function.unwrap();
        let frame = translator.functions.get_mut(&id).unwrap();
        let saved = frame.allocate(&temp, ptr);
        let mut stmts = vec![];
        let address = if cls.is_some_and(|cls| Symbol::has_subclass(cls, translator.symbols)) {
            // This call could be overridden by a subclass in which case we need to use dynamic dispatch.
            // we just checked is_some_and, so this is safe.
            let cls = cls.unwrap();
            let descriptor: &Expr = args.first().unwrap();
            let arr = Temp::next();
            let address = Temp::next();
            let (_, n) = name.rsplit_once('.').unwrap();
            let index =
                F::word_size() * (cls.methods.iter().position(|m| m.name == n).unwrap() + 1);
            stmts.append(&mut vec![
                Move::wrapped(
                    Temp::wrapped(arr.clone()),
                    Binary::wrapped(
                        BinOp::Plus,
                        descriptor.clone(),
                        Const::<i64>::int(F::word_size().try_into().unwrap()),
                    ),
                ),
                Stmt::checked_move(
                    Temp::wrapped(address.clone()),
                    Mem::wrapped(Binary::wrapped(
                        BinOp::Plus,
                        Temp::wrapped(arr.clone()),
                        Const::<i64>::int(index.try_into().unwrap()),
                    )),
                ),
            ]);
            address
        } else {
            name
        };
        stmts.append(&mut vec![
            Stmt::Expr(Box::new(Call::wrapped(address, args))),
            Move::wrapped(saved.clone(), Temp::wrapped(r.ret.into())),
        ]);
        ESeq::wrapped(Stmt::from(&stmts[..]), saved)
    }
}

impl Translate<Expr> for ast::node::Ident {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Expr {
        translator.frame().get(&self.name.to_string())
    }
}

impl Translate<Expr> for ast::node::Unary {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Expr {
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
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Expr {
        let meta = translator.meta.access.get(&self.id).unwrap();
        let head = self.chain.first().unwrap();
        let mut initial = vec![];
        let ident = match head {
            AstExpr::Ident(ident) => ident.name.to_string(),
            AstExpr::Init(init) => {
                // Invent an "anonymous" variable to hold the value of the initializer
                let name = Temp::next();
                let decl = ast::node::VarDecl::wrapped(
                    Token::new(Kind::Identifier, Some(name.clone().leak()), Span::default()),
                    Type::new(
                        Token::new(Kind::Literal, init.name.lexeme, Span::default()),
                        vec![],
                    ),
                    head.clone(),
                );
                let stmt = decl.translate(translator);
                initial.push(stmt);
                name
            }
            _ => unimplemented!(),
        };
        let frame = translator.frame();
        let base = frame.get(&ident);
        let temp = Temp::next();
        initial.append(&mut vec![Stmt::checked_move(
            Temp::wrapped(temp.clone()),
            base,
        )]);
        let stmts: Vec<_> = initial
            .into_iter()
            .chain(meta.indices.iter().map(|&field| {
                let offset: i64 = ((field + runtime::CLASS_METADATA_FIELDS) * F::word_size())
                    .try_into()
                    .unwrap();
                let mem = Mem::new(Binary::wrapped(
                    BinOp::Plus,
                    Temp::wrapped(temp.clone()),
                    Const::<i64>::int(offset),
                ));
                translator.ctx.mem = Some(mem.clone());
                Stmt::checked_move(Temp::wrapped(temp.clone()), Expr::Mem(mem))
            }))
            .collect();
        translator.ctx.stmts = stmts.clone();
        ESeq::wrapped(Stmt::from(&stmts[..]), Temp::wrapped(temp))
    }
}

impl Translate<Expr> for ast::node::Init {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Expr {
        let r = F::registers();
        let id = translator.function.unwrap();
        let frame = translator.functions.get_mut(&id).unwrap();
        let name = translator.ctx.name.join(".");
        let base = frame.allocate(&name, true);
        let cls = &translator.symbols[&self.name.to_string()];
        let fields = cls.fields(translator.symbols);
        let (descriptor, method_descriptor) = cls.descriptor(translator.symbols);
        let ptr = translator.ctx.constants.add(vec![descriptor]);
        let array_ptr = translator
            .ctx
            .constants
            .add(vec![method_descriptor.len().to_string()]);
        let array = Temp::next();
        let mut setup = vec![
            Stmt::Expr(Box::new(Call::wrapped(
                "alloc".into(),
                vec![
                    Expr::ConstStr(ptr),
                    Temp::wrapped(r.frame.to_string()),
                    Const::<i64>::int(frame.offset().sub(
                        // This is an odd offset to require, but any less than this causes the GC to miss
                        // some reachable pointers on the stack. Something to do with field initialization order
                        // perhaps?
                        i64::try_from((self.initializers.len() * 2 + 1) * F::word_size()).unwrap(),
                    )),
                ],
            ))),
            Stmt::checked_move(base.clone(), Temp::wrapped(r.ret.to_string())),
            // Allocate an array to hold the method descriptor
            Stmt::Expr(Box::new(Call::wrapped(
                "init_array".into(),
                vec![
                    Expr::ConstStr(array_ptr),
                    Temp::wrapped(r.frame.to_string()),
                    Const::<i64>::int(frame.offset().sub(
                        i64::try_from((self.initializers.len() * 2 + 1) * F::word_size()).unwrap(),
                    )),
                ],
            ))),
            Stmt::checked_move(
                Temp::wrapped(array.clone()),
                Temp::wrapped(r.ret.to_string()),
            ),
        ];
        // Initialize the method descriptor array
        for (i, method) in method_descriptor.iter().enumerate() {
            let offset: i64 = ((i + 1) * F::word_size()).try_into().unwrap();
            setup.push(Move::wrapped(
                Binary::wrapped(
                    BinOp::Plus,
                    Temp::wrapped(array.clone()),
                    Const::<i64>::int(offset),
                ),
                Expr::ConstLabel(method.clone()),
            ));
        }
        // Initialize fields in the correct order
        let mut initializers: Vec<_> = self.initializers.iter().collect();
        initializers.sort_by(|a, b| {
            let position = |name: &Token| fields.iter().position(|f| f.name == *name);
            position(&a.name).cmp(&position(&b.name))
        });
        let temp = Temp::next();
        // Hold a pointer to the method descriptor array at class_ptr[8]
        setup.append(&mut vec![
            Stmt::checked_move(Temp::wrapped(temp.clone()), base.clone()),
            Stmt::checked_move(
                Binary::wrapped(
                    BinOp::Plus,
                    Temp::wrapped(temp.clone()),
                    Const::<i64>::int(F::word_size().try_into().unwrap()),
                ),
                Temp::wrapped(array),
            ),
        ]);
        // Initialize class fields
        let stmts: Vec<_> = setup
            .into_iter()
            .chain(initializers.iter().enumerate().map(|(i, init)| {
                translator.ctx.name.push(init.name.to_string());
                let value = match &init.expr {
                    AstExpr::Init(init) => init.translate(translator),
                    _ => init.expr.translate(translator),
                };
                let offset: i64 = ((i + runtime::CLASS_METADATA_FIELDS) * F::word_size())
                    .try_into()
                    .unwrap();
                translator.ctx.name.pop();
                Stmt::checked_move(
                    Binary::wrapped(
                        BinOp::Plus,
                        Temp::wrapped(temp.clone()),
                        Const::<i64>::int(offset),
                    ),
                    value,
                )
            }))
            .collect();
        ESeq::wrapped(Stmt::from(&stmts[..]), base)
    }
}

impl Translate<Stmt> for ast::node::If {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
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
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
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
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
        let range = self.iter.range();
        let cur = ast::node::Ident::wrapped(self.index.clone());
        let start = ast::node::VarDecl::wrapped(
            cur.clone().ident().name.clone(),
            Type::new(
                Token::new(Kind::Literal, Some("int"), Span::default()),
                vec![],
            ),
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
                .chain([ast::node::Assign::wrapped(
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
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
        let target: Expr = self.target.translate(translator);
        translator.ctx.stmts.pop();
        let target = if matches!(self.target, AstExpr::Access(_)) {
            ESeq::wrapped(
                Stmt::from(&translator.ctx.stmts[..]),
                Expr::Mem(translator.ctx.mem.take().unwrap()),
            )
        } else {
            target
        };
        translator.ctx.stmts.clear();
        Stmt::checked_move(target, self.expr.translate(translator))
    }
}

impl Translate<Stmt> for AstExpr {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
        Stmt::Expr(Box::new(self.translate(translator)))
    }
}

impl Translate<Stmt> for ast::node::Return {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
        let r = F::registers();
        translator.ctx.ret = true;
        Stmt::checked_move(
            Temp::wrapped(r.ret.to_string()),
            self.expr.translate(translator),
        )
    }
}

impl Translate<Stmt> for ast::node::VarDecl {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
        let name = self.name.to_string();
        if matches!(self.expr, AstExpr::Init(_)) {
            translator.ctx.name.push(name.clone());
        }
        let expr = self.expr.translate(translator);
        translator.ctx.name.clear();
        let id = translator.function.unwrap();
        let frame = translator.functions.get_mut(&id).unwrap();
        // No matter what, variables are always F::word_size() (either pointer to first element or the value itself)
        let target = frame.allocate(
            &name,
            !matches!(self.ty.base.lexeme, Some("int" | "float" | "bool")),
        );
        Stmt::checked_move(target, expr)
    }
}

impl Translate<Stmt> for ast::node::FuncDecl {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Stmt {
        let frame = F::new(self);
        let r = F::registers();
        translator.functions.insert(self.id, frame);
        translator.function = Some(self.id);
        let mut stmts: Vec<Stmt> = vec![Label::wrapped(self.name.to_string())]
            .into_iter()
            .chain(self.body.iter().map(|stmt| stmt.translate(translator)))
            .collect();
        if self.ty.is_none() || self.ty.as_ref().unwrap().base.lexeme == Some("void") {
            // If the function returns void, explicitly zero out the return register. This can
            // cause unwanted behavior in the garbage collector because if the last call is an
            // allocation: that pointer will be copied to the parent frame and be considered
            // live by the garbage collector.
            stmts.push(Move::wrapped(
                Temp::wrapped(r.ret.to_string()),
                Const::<i64>::int(0),
            ));
        }
        Stmt::from(&stmts[..])
    }
}

impl Translate<Vec<Stmt>> for ast::node::ClassDecl {
    fn translate<I: ArchInstr, F: Frame<I>>(&self, translator: &mut Translator<I, F>) -> Vec<Stmt> {
        self.methods
            .iter()
            .map(|method| {
                let mut clone = FuncDecl::new(
                    method.name.clone(),
                    method.params.clone(),
                    method.ty.clone(),
                    method.tp.clone(),
                    method.body.clone(),
                    false,
                );
                clone.id = method.id;
                let lexeme = || -> &'static str { format!("{}.{}", self.name, clone.name).leak() };
                clone.name = Token::new(Kind::Identifier, Some(lexeme()), Span::default());
                clone.translate(translator)
            })
            .collect()
    }
}

crate::newtype!(Constants: HashMap<String, Vec<String>>);

impl Constants {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn add(&mut self, values: Vec<String>) -> String {
        if let Some((key, _)) = self.0.iter().find(|&(_, v)| *v == values) {
            key.to_owned()
        } else {
            let id = self.0.len();
            let name = format!(".const.{id}");
            self.0.insert(name.clone(), values);
            name
        }
    }
}

use crate::{
    ast::{node, span::Combined, ty::Type, Decl, Expr, Stmt},
    error::PreciseError,
    pass::{Symbol, SymbolTable},
    token::{Kind, Span, Token},
    Source,
};
use std::{collections::HashMap, rc::Rc};

#[derive(thiserror::Error, Debug)]
pub enum TypeError {
    #[error("undefined variable")]
    Undefined,
    #[error("`{0}` is not a {1}")]
    NotType(Token, &'static str),
    #[error("cannot {0} {1}")]
    UnaryMismatch(&'static str, Type),
    #[error("expected {0}, got {1}")]
    Mismatch(String, String),
    #[error("{0:?} is not a property of {1}")]
    NotProperty(Expr, Type),
}

#[derive(Debug)]
pub struct Access {
    pub symbols: Vec<Symbol>,
    pub indices: Vec<usize>,
    pub ty: Type,
}

impl Access {
    pub fn new(symbols: Vec<Symbol>, indices: Vec<usize>, ty: Type) -> Self {
        Self {
            symbols,
            indices,
            ty,
        }
    }
}

struct TypeResolverContext<'a> {
    source: &'a Source,
    symbols: &'a SymbolTable,
    errors: Vec<PreciseError<'a>>,
    scopes: Vec<SymbolTable>,
    function: Option<Token>,
    class: Option<Token>,
}

#[derive(Debug)]
struct ResolvedType {
    base: Symbol,
    #[allow(dead_code)]
    params: Vec<ResolvedType>,
    meta: Type,
}

impl ResolvedType {
    fn new(base: Symbol, params: Vec<ResolvedType>, meta: Type) -> Self {
        Self { base, params, meta }
    }

    fn field(&self, symbols: &SymbolTable, field: &Expr) -> Option<(usize, node::Field)> {
        match (&self.base, field) {
            (Symbol::Class(_), Expr::Ident(ident)) => self
                .base
                .fields(symbols)
                .iter()
                .enumerate()
                .find(|(_, f)| f.name.to_string() == ident.name.to_string())
                .map(|(i, f)| (i, f.clone())),
            _ => None,
        }
    }

    fn method(&self, symbols: &SymbolTable, method: &Expr) -> Option<Rc<node::FuncDecl>> {
        match (&self.base, method) {
            #[allow(clippy::cmp_owned)]
            (Symbol::Class(_), Expr::Call(call)) => self
                .base
                .methods(symbols)
                .iter()
                .map(|(label, method)| (label.rsplit_once('.').unwrap().1, method))
                .find(|(name, _)| *name == call.left.ident().name.to_string())
                .map(|(_, func)| func)
                .cloned(),
            _ => None,
        }
    }

    fn is_numeric(&self) -> bool {
        matches!(self.base, Symbol::Int | Symbol::Float)
    }

    fn is_bool(&self) -> bool {
        matches!(self.base, Symbol::Bool)
    }

    fn fake_meta(lexeme: &'static str) -> Type {
        Type::new(
            Token::new(Kind::Identifier, Some(lexeme), Span::default()),
            vec![],
        )
    }

    fn str() -> Self {
        Self::new(Symbol::Str, vec![], Self::fake_meta("str"))
    }

    fn float() -> Self {
        Self::new(Symbol::Float, vec![], Self::fake_meta("float"))
    }

    fn int() -> Self {
        Self::new(Symbol::Int, vec![], Self::fake_meta("int"))
    }

    fn bool() -> Self {
        Self::new(Symbol::Bool, vec![], Self::fake_meta("bool"))
    }

    fn void() -> Self {
        Self::new(Symbol::Void, vec![], Self::fake_meta("void"))
    }
}

impl PartialEq for ResolvedType {
    fn eq(&self, other: &Self) -> bool {
        match &self.base {
            Symbol::Int | Symbol::Float | Symbol::Str | Symbol::Bool | Symbol::Void => {
                self.meta.base.to_string() == other.meta.base.to_string()
            }
            Symbol::Class(cls) => {
                let other = other.meta.to_string();
                let cls = cls.name.to_string();
                cls == other
            }
            Symbol::Constant(c) => {
                let other = other.meta.to_string();
                let c = c.name.to_string();
                c == other
            }
            Symbol::Function(f) => {
                let other = other.meta.to_string();
                let f = f.name.to_string();
                f == other
            }
            Symbol::Variable(v) => {
                let other = other.meta.to_string();
                let v = v.name.to_string();
                v == other
            }
        }
    }
}

trait ResolveType {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError>;
}

impl ResolveType for Type {
    #[allow(clippy::only_used_in_recursion)]
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        Ok(ResolvedType::new(
            if let Some(symbol) = cx.symbol(&self.base.to_string()) {
                symbol.clone()
            } else {
                cx.error(
                    self.base.span,
                    format!("`{}` is not defined", self.base.lexeme.unwrap()),
                    String::new(),
                );
                return Err(TypeError::Undefined);
            },
            self.params
                .iter()
                .map(|p| p.resolve(cx, meta).unwrap())
                .collect(),
            self.clone(),
        ))
    }
}

impl ResolveType for Decl {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        match self {
            Decl::Function(fun) => fun.resolve(cx, meta),
            Decl::Class(cls) => cls.resolve(cx, meta),
            Decl::Constant(c) => c.resolve(cx, meta),
        }
    }
}

impl ResolveType for Stmt {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        match self {
            Stmt::Var(v) => v.resolve(cx, meta),
            Stmt::Assign(a) => a.resolve(cx, meta),
            Stmt::Return(r) => r.resolve(cx, meta),
            Stmt::Expr(e) => e.resolve(cx, meta),
            Stmt::If(i) => i.resolve(cx, meta),
            Stmt::While(w) => w.resolve(cx, meta),
            Stmt::For(f) => f.resolve(cx, meta),
        }
    }
}

impl ResolveType for Expr {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        match self {
            Expr::Int(i) => i.resolve(cx, meta),
            Expr::Float(f) => f.resolve(cx, meta),
            Expr::Str(s) => s.resolve(cx, meta),
            Expr::Bool(b) => b.resolve(cx, meta),
            Expr::Range(r) => r.resolve(cx, meta),
            Expr::Call(c) => c.resolve(cx, meta),
            Expr::Ident(i) => i.resolve(cx, meta),
            Expr::Unary(u) => u.resolve(cx, meta),
            Expr::Binary(b) => b.resolve(cx, meta),
            Expr::Access(a) => a.resolve(cx, meta),
            Expr::Init(i) => i.resolve(cx, meta),
        }
    }
}

impl ResolveType for node::ClassDecl {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        cx.class = Some(self.name.clone());
        for method in &self.methods {
            let _ = Decl::Function(Rc::clone(method)).resolve(cx, meta);
        }
        cx.class = None;
        Ok(ResolvedType::void())
    }
}

impl ResolveType for Rc<node::FuncDecl> {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        if self.name == "main" {
            if let Some(ty) = &self.ty {
                if !matches!(ty.resolve(cx, meta)?.base, Symbol::Void) {
                    cx.error(
                        ty.span(),
                        "main function must return void".into(),
                        "try changing or removing this type".into(),
                    );
                }
            }
        }
        cx.begin_scope();
        cx.function = Some(self.name.clone());
        if self.params.len() > 8 {
            cx.error(
                self.name.span,
                "functions cannot have more than 8 parameters".into(),
                "try removing some parameters".into(),
            );
        }
        let self_param = self.params.iter().position(|p| p.name == "self");
        if cx.class.is_some() && (self_param.is_none() || self_param.unwrap() != 0) {
            cx.error(
                self.name.span,
                "first parameter must be `self`".into(),
                "try adding `self` as the first parameter".into(),
            );
        }
        for param in &self.params {
            cx.scope_mut()
                .insert(param.name.to_string(), Symbol::Function(Rc::clone(self)));
        }
        for node in &self.body {
            let _ = node.resolve(cx, meta);
        }
        cx.end_scope();
        cx.function = None;
        Ok(ResolvedType::void())
    }
}

impl ResolveType for node::ConstantDecl {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let got = self.expr.resolve(cx, meta)?;
        let expected = self.ty.resolve(cx, meta)?;
        if got != expected {
            cx.error(
                self.expr.span(),
                format!("expected initializer to be of type {}", expected.meta),
                format!("expression of type {}", got.meta),
            );
        }
        Ok(expected)
    }
}

impl ResolveType for Rc<node::VarDecl> {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let got = self.expr.resolve(cx, meta)?;
        let expected = self.ty.resolve(cx, meta)?;
        if !matches!(
            got.base,
            Symbol::Bool | Symbol::Int | Symbol::Float | Symbol::Str | Symbol::Void
        ) {
            if got != expected && cx.cast(&expected, &got).is_none() {
                cx.error(
                    self.expr.span(),
                    format!("{} is not a subclass of {}", got.meta, expected.meta),
                    format!("expression of type {}", got.meta),
                );
            }
        } else if got != expected {
            cx.error(
                self.expr.span(),
                format!("expected initializer to be of type {}", expected.meta),
                format!("expression of type {}", got.meta),
            );
        }
        cx.scope_mut()
            .insert(self.name.to_string(), Symbol::Variable(Rc::clone(self)));
        Ok(expected)
    }
}

impl ResolveType for node::For {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        self.iter.resolve(cx, meta)?;
        cx.begin_scope();
        cx.scope_mut().insert(
            self.index.to_string(),
            Symbol::Variable(Rc::new(node::VarDecl {
                name: self.index.clone(),
                ty: Type::new(
                    Token::new(Kind::Identifier, Some("int"), Span::default()),
                    vec![],
                ),
                expr: self.iter.clone(),
            })),
        );
        for node in &self.body {
            let _ = node.resolve(cx, meta);
        }
        cx.end_scope();
        Ok(ResolvedType::void())
    }
}

impl ResolveType for node::While {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let got = self.condition.resolve(cx, meta)?;
        if !got.is_bool() {
            cx.error(
                self.condition.span(),
                "expected condition of type bool".into(),
                format!("expression of type {}", got.meta),
            );
        }
        cx.begin_scope();
        for stmt in &self.body {
            let _ = stmt.resolve(cx, meta);
        }
        cx.end_scope();
        Ok(ResolvedType::void())
    }
}

impl ResolveType for node::If {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let got = self.condition.resolve(cx, meta)?;
        if !got.is_bool() {
            cx.error(
                self.condition.span(),
                "expected condition of type bool".into(),
                format!("expression of type {}", got.meta),
            );
        }
        cx.begin_scope();
        for stmt in &self.is {
            let _ = stmt.resolve(cx, meta);
        }
        cx.end_scope();
        cx.begin_scope();
        for stmt in &self.otherwise {
            let _ = stmt.resolve(cx, meta);
        }
        cx.end_scope();

        Ok(ResolvedType::void())
    }
}

impl ResolveType for node::Unary {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let got = self.expr.resolve(cx, meta)?;
        match self.op.kind {
            Kind::Minus => {
                if !got.is_numeric() {
                    cx.error(
                        self.expr.span(),
                        format!("cannot negate {}", got.meta),
                        format!("expression of type {}", got.meta),
                    );
                    return Err(TypeError::UnaryMismatch("negate", got.meta));
                }
                Ok(got)
            }
            Kind::Bang => {
                if !got.is_bool() {
                    cx.error(
                        self.expr.span(),
                        format!("cannot invert {}", got.meta),
                        format!("expression of type {}", got.meta),
                    );
                    return Err(TypeError::UnaryMismatch("invert", got.meta));
                }
                Ok(ResolvedType::bool())
            }
            _ => unimplemented!(),
        }
    }
}

impl ResolveType for node::Call {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let function = match &*self.left {
            Expr::Ident(ident) => {
                let name = ident.name.to_string();
                match cx.symbol(&name) {
                    Some(Symbol::Function(f)) => f,
                    Some(_) => {
                        cx.error(
                            ident.name.span,
                            format!("`{name}` is not a function"),
                            String::new(),
                        );
                        return Err(TypeError::NotType(ident.name.clone(), "function"));
                    }
                    None => {
                        cx.error(
                            ident.name.span,
                            format!("`{name}` is not defined"),
                            String::new(),
                        );
                        return Err(TypeError::Undefined);
                    }
                }
            }
            Expr::Access(access) => {
                let _ = self.left.resolve(cx, meta);
                let Some(meta) = meta.access.get(&access.id) else {
                    return Err(TypeError::Undefined);
                };
                meta.symbols.last().unwrap().function()
            }
            _ => unimplemented!(),
        };
        let (arity, params, ty) = (
            function.params.len(),
            function.params.clone(),
            function.ty.clone(),
        );
        if arity != self.args.len() {
            cx.error(
                self.left.span(),
                format!(
                    "this function takes {} arguments, but {} were provided",
                    arity,
                    self.args.len()
                ),
                "while calling function here".into(),
            );
        }
        for (i, arg) in self.args.iter().enumerate() {
            let got = arg.resolve(cx, meta)?;
            if i < params.len() {
                let expected = params[i].ty.resolve(cx, meta)?;
                if !matches!(
                    got.base,
                    Symbol::Bool | Symbol::Int | Symbol::Float | Symbol::Str
                ) {
                    let casted = cx.cast(&expected, &got);
                    if got != expected && casted.is_none() {
                        cx.error(
                            arg.span(),
                            format!("{} is not a subclass of {}", got.meta, expected.meta),
                            format!("expression of type {}", got.meta),
                        );
                    }
                    if let Some(cls) = casted {
                        // This call actually refers to a parent (inherited) method call, so we need
                        // to switch out the branch label later
                        meta.call.insert(self.id, cls);
                    }
                } else if got != expected {
                    cx.error(
                        arg.span(),
                        format!(
                            "expected argument of type {}, but found {}",
                            expected.meta, got.meta
                        ),
                        format!("expression of type {}", got.meta),
                    );
                }
            }
        }
        if let Some(ty) = ty {
            ty.resolve(cx, meta)
        } else {
            Ok(ResolvedType::void())
        }
    }
}

impl ResolveType for node::Init {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        if cx.symbol(&self.name.to_string()).is_none() {
            cx.error(
                self.name.span,
                format!("`{}` is not defined", self.name),
                String::new(),
            );
            return Err(TypeError::Undefined);
        }
        let fields = cx
            .symbol(&self.name.to_string())
            .unwrap()
            .fields(cx.symbols);
        for initializer in &self.initializers {
            let got = initializer.expr.resolve(cx, meta)?;
            let expected = if let Some(field) = fields.iter().find(|f| f.name == initializer.name) {
                field.ty.resolve(cx, meta)?
            } else {
                cx.error(
                    initializer.name.span,
                    format!("no field `{}` on type `{}`", initializer.name, self.name),
                    String::new(),
                );
                continue;
            };
            if got != expected {
                cx.error(
                    initializer.expr.span(),
                    format!("expected initializer to be of type {}", expected.meta),
                    format!("expression of type {}", got.meta),
                );
            }
        }
        let symbol = {
            let symbol = cx.symbol(&self.name.to_string()).cloned();
            symbol.unwrap()
        };
        Ok(ResolvedType::new(
            symbol,
            vec![],
            Type::new(self.name.clone(), vec![]),
        ))
    }
}

impl ResolveType for node::Range {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let start = self.start.resolve(cx, meta)?;
        let end = self.end.resolve(cx, meta)?;
        if start == end && matches!(start.base, Symbol::Int) {
            Ok(ResolvedType::int())
        } else {
            cx.error(
                self.brackets.0.span,
                "expected range to be of type [int, int]".into(),
                format!("expression of [{}, {}]", start.meta, end.meta),
            );
            Err(TypeError::Mismatch(
                String::from("int"),
                start.meta.to_string(),
            ))
        }
    }
}

impl ResolveType for node::Assign {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let expected = self.target.resolve(cx, meta)?;
        let got = self.expr.resolve(cx, meta)?;
        if got != expected {
            cx.error(
                self.expr.span(),
                format!("expected expression of type {}", expected.meta),
                format!("expression of type {}", got.meta),
            );
        }
        Ok(ResolvedType::void())
    }
}

impl ResolveType for node::Access {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let mut symbols = vec![];
        let mut indices = vec![];
        let mut ty = self.chain[0].resolve(cx, meta)?;
        for (n, window) in self.chain.windows(2).enumerate() {
            let left = &window[0];
            let right = &window[1];
            let left = if n == 0 { left.resolve(cx, meta)? } else { ty };
            if let Some((index, field)) = left.field(cx.symbols, right) {
                symbols.push(left.base.clone());
                indices.push(index);
                ty = field.ty.resolve(cx, meta)?;
            } else if let Some(method) = left.method(cx.symbols, right) {
                symbols.push(left.base.clone());
                symbols.push(Symbol::Function(Rc::clone(&method)));
                ty = match &method.ty {
                    Some(ty) => ty.resolve(cx, meta)?,
                    None => ResolvedType::void(),
                };
            } else {
                cx.error(
                    right.span(),
                    format!(
                        "undefined reference to `{}` (while reading `{}`)",
                        right.ident().name,
                        left.meta
                    ),
                    String::new(),
                );
                return Err(TypeError::NotProperty(right.clone(), left.meta));
            }
        }
        meta.access
            .insert(self.id, Access::new(symbols, indices, ty.meta.clone()));
        Ok(ty)
    }
}

impl ResolveType for node::Binary {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let lhs = self.left.resolve(cx, meta)?;
        let rhs = self.right.resolve(cx, meta)?;
        if lhs != rhs {
            let lhs = lhs.meta;
            let rhs = rhs.meta;
            let heading = match self.op.kind {
                Kind::Plus => format!("cannot add {lhs} to {rhs}"),
                Kind::Minus => format!("cannot subtract {rhs} from {lhs}"),
                Kind::Star => format!("cannot multiply {lhs} by {rhs}"),
                Kind::Slash => format!("cannot divide {lhs} by {rhs}"),
                _ => format!("cannot compare {lhs} and {rhs}"),
            };
            cx.error(self.op.span, heading, String::new());
            return Err(TypeError::Mismatch(lhs.to_string(), rhs.to_string()));
        }
        if matches!(
            self.op.kind,
            Kind::Plus | Kind::Minus | Kind::Star | Kind::Slash
        ) {
            Ok(lhs)
        } else {
            Ok(ResolvedType::bool())
        }
    }
}

impl ResolveType for node::Return {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        let got = self.expr.resolve(cx, meta)?;
        match &cx.function {
            Some(function) => {
                let symb = cx
                    .class
                    .as_ref()
                    .map_or(function.to_string(), ToString::to_string);
                let symbol = cx.symbol(&symb).unwrap().clone();
                let expected = match symbol {
                    Symbol::Class(cls) => {
                        let method = cls.methods.iter().find(|m| &m.name == function).unwrap();
                        method
                            .ty
                            .as_ref()
                            .map_or(ResolvedType::void(), |t| t.resolve(cx, meta).unwrap())
                    }
                    Symbol::Function(f) => {
                        f.ty.as_ref()
                            .map_or(ResolvedType::void(), |t| t.resolve(cx, meta).unwrap())
                    }
                    _ => unimplemented!(),
                };
                if got != expected {
                    cx.error(
                        self.expr.span(),
                        format!("expected return type to be {}", expected.meta),
                        format!("expression is of type {}", got.meta),
                    );
                    return Err(TypeError::Mismatch(
                        expected.meta.base.to_string(),
                        got.meta.base.to_string(),
                    ));
                }
            }
            None => unimplemented!("disallowed by parser"),
        }
        Ok(got)
    }
}

impl ResolveType for node::Ident {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        match cx.symbol(&self.name.to_string()).cloned() {
            Some(Symbol::Function(f)) => {
                let param = f.params.iter().find(|p| p.name == self.name).unwrap();
                param.ty.resolve(cx, meta)
            }
            Some(Symbol::Variable(v)) => v.ty.resolve(cx, meta),
            Some(Symbol::Constant(c)) => c.ty.resolve(cx, meta),
            _ => {
                cx.error(
                    self.name.span,
                    format!("`{}` is not defined", &self.name),
                    String::new(),
                );
                Err(TypeError::Undefined)
            }
        }
    }
}

impl ResolveType for node::Literal<bool> {
    fn resolve(
        &self,
        _: &mut TypeResolverContext,
        _: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        Ok(ResolvedType::bool())
    }
}

impl ResolveType for node::Literal<i64> {
    fn resolve(
        &self,
        _: &mut TypeResolverContext,
        _: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        Ok(ResolvedType::int())
    }
}

impl ResolveType for node::Literal<f64> {
    fn resolve(
        &self,
        _: &mut TypeResolverContext,
        _: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        Ok(ResolvedType::float())
    }
}

impl ResolveType for node::Literal<&'static str> {
    fn resolve(
        &self,
        _: &mut TypeResolverContext,
        _: &mut ResolvedMetaInfo,
    ) -> Result<ResolvedType, TypeError> {
        Ok(ResolvedType::str())
    }
}

#[derive(Debug)]
pub struct ResolvedMetaInfo {
    pub access: HashMap<usize, Access>,
    pub call: HashMap<usize, String>,
}

impl ResolvedMetaInfo {
    pub fn new() -> Self {
        Self {
            access: HashMap::new(),
            call: HashMap::new(),
        }
    }
}

pub fn resolve_types<'a>(
    source: &'a Source,
    symbols: &'a SymbolTable,
    program: &'a Vec<Decl>,
) -> Result<ResolvedMetaInfo, Vec<PreciseError<'a>>> {
    let mut cx = TypeResolverContext::new(source, symbols);
    let mut meta = ResolvedMetaInfo::new();
    for node in program {
        let _ = node.resolve(&mut cx, &mut meta);
    }
    let len = cx.errors.len();
    if len > 0 {
        Err(cx.errors)
    } else {
        Ok(meta)
    }
}

impl<'a> TypeResolverContext<'a> {
    pub fn new(source: &'a Source, symbols: &'a SymbolTable) -> Self {
        Self {
            source,
            symbols,
            errors: vec![],
            class: None,
            function: None,
            scopes: vec![],
        }
    }

    fn scope_mut(&mut self) -> &mut SymbolTable {
        self.scopes.last_mut().unwrap()
    }

    fn begin_scope(&mut self) {
        self.scopes.push(SymbolTable::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn symbol(&self, name: &String) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(definition) = scope.get(name) {
                return Some(definition);
            }
        }
        match &name[..] {
            "int" => Some(&Symbol::Int),
            "float" => Some(&Symbol::Float),
            "str" => Some(&Symbol::Str),
            "bool" => Some(&Symbol::Bool),
            "void" => Some(&Symbol::Void),
            _ => self.symbols.get(name),
        }
    }

    fn error(&mut self, at: Span, heading: String, text: String) {
        let error = PreciseError::new(self.source, at, heading, text);
        println!("{error}");
        self.errors.push(error);
    }

    fn cast(&self, expected: &ResolvedType, got: &ResolvedType) -> Option<String> {
        let cls = self.symbol(&got.meta.to_string())?;
        let cls = cls.class();
        Symbol::superclasses(cls, self.symbols)
            .iter()
            .filter(|c| c.name != cls.name)
            .map(|c| c.name.to_string())
            .find(|cls| cls == &expected.meta.to_string())
    }
}

macro_rules! assert_typecheck {
    ($($path:expr => $name:ident),*) => {
        #[cfg(test)]
        mod tests {
            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let source = crate::Source::new($path)?;
                    let ast = crate::ast::Ast::try_from(&source)?;
                    let symbols = crate::pass::SymbolTable::from(&ast.nodes);
                    let errors = crate::pass::resolve_types(&source, &symbols, &ast.nodes);
                    insta::with_settings!({snapshot_path => "../../snapshots"}, {
                        insta::assert_debug_snapshot!(errors);
                    });
                    Ok(())
                }
            )*
        }
    };
}

assert_typecheck! {
    "test-cases/typecheck/varied.kya" => varied,
    "test-cases/typecheck/classes.kya" => classes
}

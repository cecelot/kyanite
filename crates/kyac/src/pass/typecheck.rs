use crate::{
    ast::{
        node::{self, Ident},
        span::Combined,
        Decl, Expr, Stmt, Type,
    },
    error::PreciseError,
    pass::{Symbol, SymbolTable},
    token::{Kind, Span, Token},
    Source,
};
use std::{collections::HashMap, rc::Rc};

macro_rules! symbol {
    ($self:ident, $name:expr, $ty:ident, $s:literal) => {
        match $self.symbol(&$name.to_string()) {
            Some(Symbol::$ty(v)) => v.clone(),
            Some(_) => {
                $self.error(
                    $name.span,
                    format!("`{}` is not a {}", $name, $s),
                    "".into(),
                );
                return Err(TypeError::NotType($name.clone(), $s));
            }
            None => {
                $self.error($name.span, format!("`{}` is not defined", $name), "".into());
                return Err(TypeError::Undefined);
            }
        }
    };
}

macro_rules! cast {
    ($id:expr, $res:expr, $pattern:pat) => {
        match $id {
            $pattern => $res,
            _ => unimplemented!(),
        }
    };
}

#[derive(thiserror::Error, Debug)]
pub enum TypeError {
    #[error("undefined variable")]
    Undefined,
    #[error("`{0}` is not a {1}")]
    NotType(Token, &'static str),
    #[error("cannot {0} {1}")]
    UnaryMismatch(&'static str, Type),
    #[error("expected {0}, got {1}")]
    Mismatch(Type, Type),
    #[error("{0} is not a property of {1}")]
    NotProperty(Token, Type),
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

trait ResolveType {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError>;
}

impl ResolveType for Decl {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
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
    ) -> Result<Type, TypeError> {
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
    ) -> Result<Type, TypeError> {
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
    ) -> Result<Type, TypeError> {
        cx.class = Some(self.name.clone());
        for method in &self.methods {
            let _ = Decl::Function(Rc::clone(method)).resolve(cx, meta);
        }
        cx.class = None;
        Ok(Type::Void)
    }
}

impl ResolveType for Rc<node::FuncDecl> {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        if self.name == "main" {
            if let Some(ty) = &self.ty {
                if ty != "void" {
                    cx.error(
                        ty.span,
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
        for param in &self.params {
            cx.scope_mut()
                .insert(param.name.to_string(), Symbol::Function(Rc::clone(self)));
        }
        for node in &self.body {
            let _ = node.resolve(cx, meta);
        }
        cx.end_scope();
        cx.function = None;
        Ok(Type::Void)
    }
}

impl ResolveType for node::ConstantDecl {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        let got = self.expr.resolve(cx, meta)?;
        let expected = Type::from(&self.ty);
        if got != expected {
            cx.error(
                self.expr.span(),
                format!("expected initializer to be of type {expected}"),
                format!("expression of type {got}"),
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
    ) -> Result<Type, TypeError> {
        let got = self.expr.resolve(cx, meta)?;
        let expected = Type::from(&self.ty);
        if let Type::UserDefined(ref cls) = got {
            if got != expected && cx.cast(&expected, cls).is_none() {
                cx.error(
                    self.expr.span(),
                    format!("{got} is not a subclass of {expected}"),
                    format!("expression of type {got}"),
                );
            }
        } else if got != expected {
            cx.error(
                self.expr.span(),
                format!("expected initializer to be of type {expected}"),
                format!("expression of type {got}"),
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
    ) -> Result<Type, TypeError> {
        self.iter.resolve(cx, meta)?;
        cx.begin_scope();
        cx.scope_mut().insert(
            self.index.to_string(),
            Symbol::Variable(Rc::new(node::VarDecl {
                name: self.index.clone(),
                ty: Token::new(Kind::Identifier, Some("int"), self.index.span),
                expr: self.iter.clone(),
            })),
        );
        for node in &self.body {
            let _ = node.resolve(cx, meta);
        }
        cx.end_scope();
        Ok(Type::Void)
    }
}

impl ResolveType for node::While {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        let got = self.condition.resolve(cx, meta)?;
        if got != Type::Bool {
            cx.error(
                self.condition.span(),
                format!("expected condition of type {}", Type::Bool),
                format!("expression of type {got}"),
            );
        }
        cx.begin_scope();
        for stmt in &self.body {
            let _ = stmt.resolve(cx, meta);
        }
        cx.end_scope();
        Ok(Type::Void)
    }
}

impl ResolveType for node::If {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        let got = self.condition.resolve(cx, meta)?;
        if got != Type::Bool {
            cx.error(
                self.condition.span(),
                format!("expected condition of type {}", Type::Bool),
                format!("expression of type {got}"),
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

        Ok(Type::Void)
    }
}

impl ResolveType for node::Unary {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        let got = self.expr.resolve(cx, meta)?;
        match self.op.kind {
            Kind::Minus => {
                if !matches!(got, Type::Int | Type::Float) {
                    cx.error(
                        self.expr.span(),
                        format!("cannot negate {got}"),
                        format!("expression of type {got}"),
                    );
                    return Err(TypeError::UnaryMismatch("negate", got));
                }
                Ok(got)
            }
            Kind::Bang => {
                if got != Type::Bool {
                    cx.error(
                        self.expr.span(),
                        format!("cannot invert {got}"),
                        format!("expression of type {got}"),
                    );
                    return Err(TypeError::UnaryMismatch("invert", got));
                }
                Ok(Type::Bool)
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
    ) -> Result<Type, TypeError> {
        let function = match &*self.left {
            Expr::Ident(ident) => {
                let name = ident.name.to_string();
                match cx.symbol(&name) {
                    Some(Symbol::Function(f)) => f.as_ref(),
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
                let expected = Type::from(&params[i].ty);
                if let Type::UserDefined(ref cls) = got {
                    let casted = cx.cast(&expected, cls);
                    if got != expected && casted.is_none() {
                        cx.error(
                            arg.span(),
                            format!("{got} is not a subclass of {expected}"),
                            format!("expression of type {got}"),
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
                        format!("expected argument of type {expected}, but found {got}"),
                        format!("expression of type {got}"),
                    );
                }
            }
        }
        Ok(if let Some(ty) = ty {
            Type::from(&ty)
        } else {
            Type::Void
        })
    }
}

impl ResolveType for node::Init {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        symbol!(cx, self.name, Class, "class"); // ensure class is defined
        let fields = cx
            .symbol(&self.name.to_string())
            .unwrap()
            .fields(cx.symbols);
        for initializer in &self.initializers {
            let got = initializer.expr.resolve(cx, meta)?;
            let expected = if let Some(field) = fields.iter().find(|f| f.name == initializer.name) {
                Type::from(&field.ty)
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
                    format!("expected initializer to be of type {expected}"),
                    format!("expression of type {got}"),
                );
            }
        }
        Ok((&self.name).into())
    }
}

impl ResolveType for node::Range {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        let start = self.start.resolve(cx, meta)?;
        let end = self.end.resolve(cx, meta)?;
        if start == end && start == Type::Int {
            Ok(Type::Int)
        } else {
            cx.error(
                self.brackets.0.span,
                "expected range to be of type [int, int]".into(),
                format!("expression of [{start}, {end}]"),
            );
            Err(TypeError::Mismatch(Type::Int, start))
        }
    }
}

impl ResolveType for node::Assign {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        let expected = self.target.resolve(cx, meta)?;
        let got = self.expr.resolve(cx, meta)?;
        if got != expected {
            cx.error(
                self.expr.span(),
                format!("expected expression of type {expected}"),
                format!("expression of type {got}"),
            );
        }
        Ok(Type::Void)
    }
}

// TODO: make this prettier
impl ResolveType for node::Access {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        fn err(
            cx: &mut TypeResolverContext,
            kind: &str,
            ident: &Ident,
            ty: Type,
        ) -> Result<Type, TypeError> {
            cx.error(
                ident.name.span,
                format!("no {kind} `{}` on type `{}`", ident.name, ty),
                String::new(),
            );
            Err(TypeError::NotProperty(ident.name.clone(), ty))
        }
        let mut ty = self.chain[0].resolve(cx, meta)?;
        let mut symbols = vec![];
        let mut indices = vec![];
        let Some(mut symbol) = cx.symbol(&ty.to_string()).cloned() else {
            return Err(TypeError::Undefined);
        };
        symbols.push(symbol.clone());
        for (i, pair) in self.chain.windows(2).enumerate() {
            let (left, right) = (&pair[0], &pair[1]);
            if i != 0 {
                let cls = cast!(symbol, r, Symbol::Class(ref r));
                let fields = cx.symbol(&cls.name.to_string()).unwrap().fields(cx.symbols);
                if let Expr::Ident(ident) = left {
                    let field = fields.iter().find(|f| f.name == ident.name);
                    if let Some(field) = field {
                        symbol = cx.symbol(&field.ty.to_string()).cloned().unwrap();
                        symbols.push(symbol.clone());
                    } else {
                        return err(cx, "field", ident, ty);
                    }
                } else {
                    todo!("support accesses after method calls")
                }
            }
            let cls = cast!(symbol, r, Symbol::Class(ref r));
            if let Expr::Ident(ident) = right {
                let fields = cx.symbol(&cls.name.to_string()).unwrap().fields(cx.symbols);
                let index = fields.iter().position(|f| f.name == ident.name);
                if let Some(index) = index {
                    indices.push(index);
                    ty = Type::from(&fields[index].ty);
                } else {
                    return err(cx, "field", ident, ty);
                }
            } else {
                let symbol = cx.symbol(&ty.to_string()).unwrap();
                let call = cast!(right, c, Expr::Call(c));
                let ident = call.left.ident();
                let methods = symbol.methods(cx.symbols);
                let method = methods
                    .iter()
                    // make sure we find the most "specific" implementation
                    // (i.e. Y.method() before X.method())
                    .rev()
                    .find(|(_, m)| m.name.to_string() == ident.name.to_string());
                if let Some((_, method)) = method {
                    symbols.push(Symbol::Function(Rc::clone(method)));
                    ty = Type::from(method.ty.as_ref());
                } else {
                    return err(cx, "method", ident, ty);
                }
            }
        }
        meta.access
            .insert(self.id, Access::new(symbols, indices, ty.clone()));
        Ok(ty)
    }
}

impl ResolveType for node::Binary {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        let lhs = self.left.resolve(cx, meta)?;
        let rhs = self.right.resolve(cx, meta)?;
        if lhs != rhs {
            let heading = match self.op.kind {
                Kind::Plus => format!("cannot add {lhs} to {rhs}"),
                Kind::Minus => format!("cannot subtract {rhs} from {lhs}"),
                Kind::Star => format!("cannot multiply {lhs} by {rhs}"),
                Kind::Slash => format!("cannot divide {lhs} by {rhs}"),
                _ => format!("cannot compare {lhs} and {rhs}"),
            };
            cx.error(self.op.span, heading, String::new());
            return Err(TypeError::Mismatch(lhs, rhs));
        }
        if matches!(
            self.op.kind,
            Kind::Plus | Kind::Minus | Kind::Star | Kind::Slash
        ) {
            Ok(lhs)
        } else {
            Ok(Type::Bool)
        }
    }
}

impl ResolveType for node::Return {
    fn resolve(
        &self,
        cx: &mut TypeResolverContext,
        meta: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        let got = self.expr.resolve(cx, meta)?;
        match &cx.function {
            Some(function) => {
                let symb = cx
                    .class
                    .as_ref()
                    .map_or(function.to_string(), ToString::to_string);
                let symbol = cx.symbol(&symb).unwrap();
                let expected = match symbol {
                    Symbol::Class(cls) => {
                        let method = cls.methods.iter().find(|m| &m.name == function).unwrap();
                        Type::from(method.ty.as_ref())
                    }
                    Symbol::Function(f) => Type::from(f.ty.as_ref()),
                    _ => unimplemented!(),
                };
                if got != expected {
                    cx.error(
                        self.expr.span(),
                        format!("expected return type to be {expected}"),
                        format!("expression is of type {got}"),
                    );
                    return Err(TypeError::Mismatch(expected, got));
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
        _: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        Ok(match cx.symbol(&self.name.to_string()) {
            Some(Symbol::Function(f)) => {
                let param = f.params.iter().find(|p| p.name == self.name).unwrap();
                Type::from(&param.ty)
            }
            Some(Symbol::Variable(v)) => Type::from(&v.ty),
            Some(Symbol::Constant(c)) => Type::from(&c.ty),
            _ => {
                cx.error(
                    self.name.span,
                    format!("`{}` is not defined", &self.name),
                    String::new(),
                );
                return Err(TypeError::Undefined);
            }
        })
    }
}

impl ResolveType for node::Literal<bool> {
    fn resolve(
        &self,
        _: &mut TypeResolverContext,
        _: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        Ok(Type::Bool)
    }
}

impl ResolveType for node::Literal<i64> {
    fn resolve(
        &self,
        _: &mut TypeResolverContext,
        _: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        Ok(Type::Int)
    }
}

impl ResolveType for node::Literal<f64> {
    fn resolve(
        &self,
        _: &mut TypeResolverContext,
        _: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        Ok(Type::Float)
    }
}

impl ResolveType for node::Literal<&'static str> {
    fn resolve(
        &self,
        _: &mut TypeResolverContext,
        _: &mut ResolvedMetaInfo,
    ) -> Result<Type, TypeError> {
        Ok(Type::Str)
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
        self.symbols.get(name)
    }

    fn error(&mut self, at: Span, heading: String, text: String) {
        let error = PreciseError::new(self.source, at, heading, text);
        println!("{error}");
        self.errors.push(error);
    }

    fn cast(&self, expected: &Type, cls: &String) -> Option<String> {
        let cls = self.symbol(cls).unwrap().class();
        Symbol::superclasses(cls, self.symbols)
            .iter()
            .filter(|c| c.name != cls.name)
            .map(|c| c.name.to_string())
            .find(|cls| cls == &expected.to_string())
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

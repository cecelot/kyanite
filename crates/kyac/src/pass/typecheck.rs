use crate::{
    ast::{
        node::{self, Ident},
        span::Combined,
        Decl, Expr, Stmt, Type,
    },
    pass::{Symbol, SymbolTable},
    reporting::error::PreciseError,
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
                let name = $name.to_string();
                if name != "println" && name != "max" && name == "min" {
                    $self.error($name.span, format!("`{}` is not defined", $name), "".into());
                    return Err(TypeError::Undefined);
                }
                return Ok(Type::Void);
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

pub type AccessMap = HashMap<usize, Access>;

pub struct TypeCheckPass<'a> {
    program: &'a Vec<Decl>,
    symbols: &'a SymbolTable,
    accesses: &'a mut AccessMap,
    source: &'a Source,
    errors: Vec<PreciseError<'a>>,
    scopes: Vec<SymbolTable>,
    function: Option<Token>,
}

trait Check {
    fn check(&self, pass: &mut TypeCheckPass) -> Result<Type, TypeError>;
}

impl Check for Decl {
    fn check(&self, pass: &mut TypeCheckPass) -> Result<Type, TypeError> {
        match self {
            Decl::Function(fun) => Ok(pass.function(fun)),
            Decl::Constant(c) => pass.constant(c),
            Decl::Record(_) => Ok(Type::Void),
        }
    }
}

impl Check for Stmt {
    fn check(&self, pass: &mut TypeCheckPass) -> Result<Type, TypeError> {
        match self {
            Stmt::Return(r) => pass.ret(r),
            Stmt::Expr(e) => e.check(pass),
            Stmt::Var(v) => pass.var(v),
            Stmt::Assign(a) => pass.assign(a),
            Stmt::If(c) => pass.condition(c),
            Stmt::While(c) => pass.loops(c),
        }
    }
}

impl Check for Expr {
    fn check(&self, pass: &mut TypeCheckPass) -> Result<Type, TypeError> {
        match self {
            Expr::Binary(b) => pass.binary(b),
            Expr::Unary(u) => pass.unary(u),
            Expr::Call(c) => pass.call(c),
            Expr::Ident(i) => pass.ident(i),
            Expr::Access(access) => pass.access(access),
            Expr::Init(init) => pass.init(init),
            Expr::Bool(..) => Ok(Type::Bool),
            Expr::Int(..) => Ok(Type::Int),
            Expr::Float(..) => Ok(Type::Float),
            Expr::Str(..) => Ok(Type::Str),
        }
    }
}

impl<'a> TypeCheckPass<'a> {
    pub fn new(
        symbols: &'a SymbolTable,
        accesses: &'a mut AccessMap,
        source: &'a Source,
        program: &'a Vec<Decl>,
    ) -> Self {
        Self {
            source,
            program,
            symbols,
            accesses,
            errors: vec![],
            function: None,
            scopes: vec![],
        }
    }

    pub fn run(&mut self) -> Result<(), usize> {
        for node in self.program {
            let _ = node.check(self);
        }
        let len = self.errors.len();
        if len > 0 {
            Err(len)
        } else {
            Ok(())
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

    fn unary(&mut self, unary: &node::Unary) -> Result<Type, TypeError> {
        let got = unary.expr.check(self)?;
        match unary.op.kind {
            Kind::Minus => {
                if !matches!(got, Type::Int | Type::Float) {
                    self.error(
                        unary.expr.span(),
                        format!("cannot negate {got}"),
                        format!("expression of type {got}"),
                    );
                    return Err(TypeError::UnaryMismatch("negate", got));
                }
                Ok(got)
            }
            Kind::Bang => {
                if got != Type::Bool {
                    self.error(
                        unary.expr.span(),
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

    fn assign(&mut self, a: &node::Assign) -> Result<Type, TypeError> {
        let expected = a.target.check(self)?;
        let got = a.expr.check(self)?;
        if got != expected {
            self.error(
                a.expr.span(),
                format!("expected expression of type {expected}"),
                format!("expression of type {got}"),
            );
        }
        Ok(Type::Void)
    }

    fn blocks(&mut self, condition: &Expr, blocks: &[&[Stmt]]) -> Result<Type, TypeError> {
        let got = condition.check(self)?;
        if got != Type::Bool {
            self.error(
                condition.span(),
                format!("expected condition of type {}", Type::Bool),
                format!("expression of type {got}"),
            );
        }
        for block in blocks {
            for node in *block {
                let _ = node.check(self);
            }
        }
        Ok(Type::Void)
    }

    fn condition(&mut self, c: &node::If) -> Result<Type, TypeError> {
        self.blocks(&c.condition, &[&c.is, &c.otherwise])
    }

    fn loops(&mut self, c: &node::While) -> Result<Type, TypeError> {
        self.blocks(&c.condition, &[&c.body])
    }

    fn init(&mut self, init: &node::Init) -> Result<Type, TypeError> {
        let rec = symbol!(self, init.name, Record, "record");
        for initializer in &init.initializers {
            let got = initializer.expr.check(self)?;
            let expected =
                if let Some(field) = rec.fields.iter().find(|f| f.name == initializer.name) {
                    Type::from(&field.ty)
                } else {
                    self.error(
                        initializer.name.span,
                        format!("no field `{}` on type `{}`", initializer.name, init.name),
                        String::new(),
                    );
                    continue;
                };
            if got != expected {
                self.error(
                    initializer.expr.span(),
                    format!("expected initializer to be of type {expected}"),
                    format!("expression of type {got}"),
                );
            }
        }
        Ok((&init.name).into())
    }

    // TODO: make this prettier
    fn access(&mut self, access: &node::Access) -> Result<Type, TypeError> {
        fn err(pass: &mut TypeCheckPass, ident: &Ident, ty: Type) -> Result<Type, TypeError> {
            pass.error(
                ident.name.span,
                format!("no field `{}` on type `{}`", ident.name, ty),
                String::new(),
            );
            Err(TypeError::NotProperty(ident.name.clone(), ty))
        }
        let mut ty = access.chain[0].check(self)?;
        let mut symbols = vec![];
        let mut indices = vec![];
        let Some(mut symbol) = self.symbol(&ty.to_string()).cloned() else {
            return Err(TypeError::Undefined);
        };
        symbols.push(symbol.clone());
        for (i, pair) in access.chain.windows(2).enumerate() {
            let (left, right) = (&pair[0], &pair[1]);
            if i != 0 {
                // TODO: implement member functions
                let rec = cast!(symbol, r, Symbol::Record(ref r));
                let ident = cast!(left, i, Expr::Ident(i));
                let field = rec.fields.iter().find(|f| f.name == ident.name);
                if let Some(field) = field {
                    symbol = self.symbol(&field.ty.to_string()).cloned().unwrap();
                    symbols.push(symbol.clone());
                } else {
                    return err(self, ident, ty);
                }
            }
            // TODO: implement member functions (same as above)
            let rec = cast!(symbol, r, Symbol::Record(ref r));
            let ident = cast!(right, i, Expr::Ident(i));
            let index = rec.fields.iter().position(|f| f.name == ident.name);
            if let Some(index) = index {
                indices.push(index);
                ty = Type::from(&rec.fields[index].ty);
            } else {
                return err(self, ident, ty);
            }
        }
        self.accesses
            .insert(access.id, Access::new(symbols, indices, ty.clone()));
        Ok(ty)
    }

    fn constant(&mut self, c: &node::ConstantDecl) -> Result<Type, TypeError> {
        let got = c.expr.check(self)?;
        let expected = Type::from(&c.ty);
        if got != expected {
            self.error(
                c.expr.span(),
                format!("expected initializer to be of type {expected}"),
                format!("expression of type {got}"),
            );
        }
        Ok(expected)
    }

    fn var(&mut self, v: &Rc<node::VarDecl>) -> Result<Type, TypeError> {
        let got = v.expr.check(self)?;
        let expected = Type::from(&v.ty);
        if got != expected {
            self.error(
                v.expr.span(),
                format!("expected initializer to be of type {expected}"),
                format!("expression of type {got}"),
            );
        }
        self.scope_mut()
            .insert(v.name.to_string(), Symbol::Variable(Rc::clone(v)));
        Ok(expected)
    }

    fn function(&mut self, fun: &Rc<node::FuncDecl>) -> Type {
        if fun.name == "main" {
            if let Some(ty) = &fun.ty {
                if ty != "void" {
                    self.error(
                        ty.span,
                        "main function must return void".into(),
                        "try changing or removing this type".into(),
                    );
                }
            }
        }
        self.begin_scope();
        self.function = Some(fun.name.clone());
        for param in &fun.params {
            self.scope_mut()
                .insert(param.name.to_string(), Symbol::Function(Rc::clone(fun)));
        }
        for node in &fun.body {
            let _ = node.check(self);
        }
        self.end_scope();
        self.function = None;
        Type::Void
    }

    fn ret(&mut self, r: &node::Return) -> Result<Type, TypeError> {
        let got = r.expr.check(self)?;
        match &self.function {
            Some(function) => {
                let symbol = self.symbol(&function.to_string()).unwrap().clone();
                if got != symbol.ty() {
                    self.error(
                        r.expr.span(),
                        format!("expected return type to be {}", symbol.ty()),
                        format!("expression is of type {got}"),
                    );
                    return Err(TypeError::Mismatch(symbol.ty(), got));
                }
            }
            None => unimplemented!("disallowed by parser"),
        }
        Ok(got)
    }

    fn binary(&mut self, b: &node::Binary) -> Result<Type, TypeError> {
        let lhs = b.left.check(self)?;
        let rhs = b.right.check(self)?;
        if lhs != rhs {
            let heading = match b.op.kind {
                Kind::Plus => format!("cannot add {lhs} to {rhs}"),
                Kind::Minus => format!("cannot subtract {rhs} from {lhs}"),
                Kind::Star => format!("cannot multiply {lhs} by {rhs}"),
                Kind::Slash => format!("cannot divide {lhs} by {rhs}"),
                _ => format!("cannot compare {lhs} and {rhs}"),
            };
            self.error(b.op.span, heading, String::new());
            return Err(TypeError::Mismatch(lhs, rhs));
        }
        if matches!(
            b.op.kind,
            Kind::Plus | Kind::Minus | Kind::Star | Kind::Slash
        ) {
            Ok(lhs)
        } else {
            Ok(Type::Bool)
        }
    }

    fn ident(&mut self, id: &node::Ident) -> Result<Type, TypeError> {
        Ok(match self.symbol(&id.name.to_string()) {
            Some(Symbol::Function(f)) => {
                let param = f.params.iter().find(|p| p.name == id.name).unwrap();
                Type::from(&param.ty)
            }
            Some(Symbol::Variable(v)) => Type::from(&v.ty),
            Some(Symbol::Constant(c)) => Type::from(&c.ty),
            _ => {
                self.error(
                    id.name.span,
                    format!("`{}` is not defined", &id.name),
                    String::new(),
                );
                return Err(TypeError::Undefined);
            }
        })
    }

    fn call(&mut self, call: &node::Call) -> Result<Type, TypeError> {
        let name = cast!(&*call.left, ident.name.clone(), Expr::Ident(ident));
        let function = symbol!(self, name, Function, "function");
        let (arity, params, ty) = (
            function.params.len(),
            function.params.clone(),
            function.ty.clone(),
        );
        if arity != call.args.len() {
            self.error(
                call.left.span(),
                format!(
                    "this function takes {} arguments, but {} were provided",
                    arity,
                    call.args.len()
                ),
                format!("while calling {name} here"),
            );
        }
        for (i, arg) in call.args.iter().enumerate() {
            let got = arg.check(self)?;
            if i < params.len() {
                let expected = Type::from(&params[i].ty);
                if got != expected {
                    self.error(
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

macro_rules! assert_typecheck {
    ($($path:expr => $name:ident),*) => {
        #[cfg(test)]
        mod tests {
            use std::collections::HashMap;
            use crate::{SymbolTable, pass::typecheck::TypeCheckPass};

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let source = crate::Source::new($path)?;
                    let ast = crate::ast::Ast::try_from(&source)?;
                    let symbols = SymbolTable::from(&ast.nodes);
                    let mut accesses = HashMap::new();
                    let mut pass = TypeCheckPass::new(&symbols, &mut accesses, &source, &ast.nodes);
                    let _ = pass.run();
                    insta::with_settings!({snapshot_path => "../../snapshots"}, {
                        insta::assert_debug_snapshot!(pass.errors);
                    });

                    Ok(())
                }
            )*
        }
    };
}

assert_typecheck! {
    "test-cases/typecheck/varied.kya" => varied,
    "test-cases/typecheck/records.kya" => records
}

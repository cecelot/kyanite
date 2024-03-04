use crate::{
    ast::{Decl, Expr, Stmt},
    token::Token,
};
use std::{
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, PartialEq)]
pub struct FuncDecl {
    pub name: Token,
    pub params: Vec<Param>,
    pub ty: Option<Token>,
    pub body: Vec<Stmt>,
    pub external: bool,
    pub id: usize,
}

impl FuncDecl {
    pub fn new(
        name: Token,
        params: Vec<Param>,
        ty: Option<Token>,
        body: Vec<Stmt>,
        external: bool,
    ) -> Self {
        static ID: AtomicUsize = AtomicUsize::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        Self {
            name,
            params,
            ty,
            body,
            external,
            id,
        }
    }

    pub fn wrapped(
        name: Token,
        params: Vec<Param>,
        ty: Option<Token>,
        body: Vec<Stmt>,
        external: bool,
    ) -> Decl {
        Decl::Function(Rc::new(Self::new(name, params, ty, body, external)))
    }
}

#[derive(Debug, PartialEq)]
pub struct ClassDecl {
    pub name: Token,
    pub fields: Vec<Field>,
    pub methods: Vec<Rc<FuncDecl>>,
    pub parent: Option<Token>,
}

impl ClassDecl {
    pub fn wrapped(
        name: Token,
        fields: Vec<Field>,
        methods: Vec<Rc<FuncDecl>>,
        parent: Option<Token>,
    ) -> Decl {
        Decl::Class(Rc::new(Self {
            name,
            fields,
            methods,
            parent,
        }))
    }
}

#[derive(Debug)]
pub struct ConstantDecl {
    pub name: Token,
    pub ty: Token,
    pub expr: Expr,
}

impl ConstantDecl {
    pub fn wrapped(name: Token, ty: Token, expr: Expr) -> Decl {
        Decl::Constant(Rc::new(Self { name, ty, expr }))
    }
}

#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub name: Token,
    pub ty: Token,
    pub expr: Expr,
}

impl VarDecl {
    pub fn wrapped(name: Token, ty: Token, expr: Expr) -> Stmt {
        Stmt::Var(Rc::new(Self { name, ty, expr }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Assign {
    pub target: Expr,
    pub expr: Expr,
}

impl Assign {
    pub fn wrapped(target: Expr, expr: Expr) -> Stmt {
        Stmt::Assign(Rc::new(Self { target, expr }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Return {
    pub expr: Expr,
    pub keyword: Token,
}

impl Return {
    pub fn wrapped(expr: Expr, keyword: Token) -> Stmt {
        Stmt::Return(Rc::new(Self { expr, keyword }))
    }
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub condition: Expr,
    pub is: Vec<Stmt>,
    pub otherwise: Vec<Stmt>,
}

impl If {
    pub fn wrapped(condition: Expr, is: Vec<Stmt>, otherwise: Vec<Stmt>) -> Stmt {
        Stmt::If(Rc::new(Self {
            condition,
            is,
            otherwise,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub struct While {
    pub condition: Expr,
    pub body: Vec<Stmt>,
}

impl While {
    pub fn wrapped(condition: Expr, body: Vec<Stmt>) -> Stmt {
        Stmt::While(Rc::new(Self { condition, body }))
    }
}

#[derive(Debug, PartialEq)]
pub struct For {
    pub index: Token,
    pub iter: Expr,
    pub body: Vec<Stmt>,
}

impl For {
    pub fn wrapped(index: Token, range: Expr, body: Vec<Stmt>) -> Stmt {
        Stmt::For(Rc::new(Self {
            index,
            iter: range,
            body,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub left: Box<Expr>,
    pub args: Vec<Expr>,
    pub parens: (Token, Token),
    pub delimiters: Vec<Token>,
}

impl Call {
    pub fn new(
        left: Box<Expr>,
        args: Vec<Expr>,
        parens: (Token, Token),
        delimiters: Vec<Token>,
    ) -> Self {
        Self {
            left,
            args,
            parens,
            delimiters,
        }
    }

    pub fn wrapped(
        left: Expr,
        args: Vec<Expr>,
        parens: (Token, Token),
        delimiters: Vec<Token>,
    ) -> Expr {
        Expr::Call(Rc::new(Self::new(Box::new(left), args, parens, delimiters)))
    }
}

#[derive(Debug, PartialEq)]
pub struct Access {
    pub chain: Vec<Expr>,
    pub id: usize,
}

impl Access {
    pub fn wrapped(chain: Vec<Expr>) -> Expr {
        static ID: AtomicUsize = AtomicUsize::new(0);
        let id = ID.fetch_add(1, Ordering::SeqCst);
        Expr::Access(Rc::new(Self { chain, id }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

impl Binary {
    pub fn wrapped(left: Expr, op: Token, right: Expr) -> Expr {
        Expr::Binary(Rc::new(Self {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Unary {
    pub op: Token,
    pub expr: Box<Expr>,
}

impl Unary {
    pub fn wrapped(op: Token, expr: Expr) -> Expr {
        Expr::Unary(Rc::new(Self {
            op,
            expr: Box::new(expr),
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident {
    pub name: Token,
}

impl Ident {
    pub fn wrapped(name: Token) -> Expr {
        Expr::Ident(Rc::new(Self { name }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Init {
    pub name: Token,
    pub initializers: Vec<Initializer>,
    pub parens: (Token, Token),
}

impl Init {
    pub fn wrapped(name: Token, initializers: Vec<Initializer>, parens: (Token, Token)) -> Expr {
        Expr::Init(Rc::new(Self {
            name,
            initializers,
            parens,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Range {
    pub start: Expr,
    pub end: Expr,
    pub brackets: (Token, Token),
}

impl Range {
    pub fn wrapped(start: Expr, end: Expr, brackets: (Token, Token)) -> Expr {
        Expr::Range(Rc::new(Self {
            start,
            end,
            brackets,
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Literal<T> {
    pub value: T,
    pub token: Token,
}

impl<T> Literal<T> {
    pub fn new(value: T, token: Token) -> Self {
        Self { value, token }
    }

    pub fn int(value: i64, token: Token) -> Expr {
        Expr::Int(Rc::new(Literal::new(value, token)))
    }

    pub fn float(value: f64, token: Token) -> Expr {
        Expr::Float(Rc::new(Literal::new(value, token)))
    }

    pub fn string(value: &'static str, token: Token) -> Expr {
        Expr::Str(Rc::new(Literal::new(value, token)))
    }

    pub fn bool(value: bool, token: Token) -> Expr {
        Expr::Bool(Rc::new(Literal::new(value, token)))
    }
}

#[derive(Debug, PartialEq)]
pub struct Initializer {
    pub name: Token,
    pub expr: Expr,
}

impl Initializer {
    pub fn new(name: Token, expr: Expr) -> Self {
        Self { name, expr }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Token,
    pub ty: Token,
}

impl Param {
    pub fn new(name: Token, ty: Token) -> Self {
        Self { name, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: Token,
    pub ty: Token,
}

impl Field {
    pub fn new(name: Token, ty: Token) -> Self {
        Self { name, ty }
    }
}

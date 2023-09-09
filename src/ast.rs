use crate::{
    parse::{ParseError, Parser},
    token::Token,
};

use self::item::Item;

#[derive(Debug)]
pub struct Ast {
    pub root: Item,
}

impl Ast {
    pub fn new(tokens: Vec<Token>) -> Result<Self, ParseError> {
        Ok(Self {
            root: Parser::from(tokens).parse()?,
        })
    }
}

impl From<Vec<Token>> for Ast {
    fn from(tokens: Vec<Token>) -> Self {
        match Ast::new(tokens) {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Type {
    Str,
    Int,
    Float,
    Bool,
    Void,
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match value.as_str() {
            "str" => Self::Str,
            "int" => Self::Int,
            "float" => Self::Float,
            "bool" => Self::Bool,
            "void" => Self::Void,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub struct Param {
    name: String,
    ty: Type,
}

impl Param {
    pub fn new(name: String, ty: Type) -> Self {
        Self { name, ty }
    }
}

pub mod item {
    use std::fmt;

    use crate::token::Token;

    use super::{Param, Type};

    #[derive(Debug)]
    pub enum Item {
        Function(String, Vec<Param>, Type, Vec<Item>),
        Call(Box<Item>, Vec<Item>),
        Stmt(Box<Item>),
        Binary(Box<Item>, Token, Box<Item>),
        Unary(Token, Box<Item>),
        Str(String),
        Int(i64),
        Float(f64),
        Bool(bool),
        #[allow(dead_code)]
        Void,
        Ident(String),
        Eof,
    }

    impl Item {
        pub fn binary(left: Item, operator: Token, right: Item) -> Self {
            Self::Binary(Box::new(left), operator, Box::new(right))
        }
    }

    impl fmt::Display for Item {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Item::Function(name, params, ty, body) => {
                    write!(f, "defn {}(", name)?;
                    for (i, param) in params.iter().enumerate() {
                        write!(f, "{}: {:?}", param.name, param.ty)?;
                        if i < params.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "): {:?} {{\n", ty)?;
                    for item in body {
                        write!(f, "{}", item)?;
                    }
                    write!(f, "}}")
                }
                Item::Binary(left, op, right) => write!(f, "({} {} {})", left, op, right),
                Item::Call(name, args) => {
                    write!(f, "{}(", name)?;
                    for (i, arg) in args.iter().enumerate() {
                        write!(f, "{}", arg)?;
                        if i < args.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                }
                Item::Float(n) => write!(f, "{}", n),
                Item::Int(i) => write!(f, "{}", i),
                Item::Str(s) => write!(f, "{}", s),
                Item::Bool(b) => write!(f, "{}", b),
                Item::Void => write!(f, "void"),
                Item::Ident(s) => write!(f, "{}", s),
                Item::Stmt(stmt) => write!(f, "{}", stmt),
                Item::Unary(op, right) => write!(f, "({} {})", op, right),
                Item::Eof => write!(f, "EOF"),
            }
        }
    }
}

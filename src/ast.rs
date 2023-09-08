use crate::{
    parse::{ParseError, Parser},
    token::Token,
};

use self::item::Item;

#[derive(Debug)]
pub struct Ast {
    root: Item,
}

impl Ast {
    pub fn new(tokens: Vec<Token>) -> Result<Self, ParseError> {
        Ok(Self {
            root: Parser::from(tokens).parse()?,
        })
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
pub struct Stmt {}
pub mod item {
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
        Void,
        Ident(String),
    }

    impl Item {
        pub fn binary(left: Item, operator: Token, right: Item) -> Self {
            Self::Binary(Box::new(left), operator, Box::new(right))
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

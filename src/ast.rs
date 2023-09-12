use crate::{
    parse::{ParseError, Parser},
    token::Token,
};
use std::fmt;

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

#[derive(Debug)]
pub enum Item {
    Program(Vec<Item>),
    Function(String, Vec<Param>, Type, Box<Item>),
    Assign(Box<Item>, Box<Item>),
    Decl(String, Type, Box<Item>),
    Block(Vec<Item>),
    Call(Box<Item>, Vec<Item>),
    Return(Box<Item>),
    Binary(Box<Item>, Token, Box<Item>),
    Unary(Token, Box<Item>),
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    #[allow(dead_code)]
    Void,
    Ident(String),
}

impl Item {
    pub fn binary(left: Item, operator: Token, right: Item) -> Self {
        Self::Binary(Box::new(left), operator, Box::new(right))
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Program(items) => {
                for item in items {
                    writeln!(f, "{}", item)?;
                }
                Ok(())
            }
            Item::Function(name, params, ty, body) => {
                write!(f, "defn {}(", name)?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}: {:?}", param.name, param.ty)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                writeln!(f, "): {:?} {{", ty)?;
                match body.as_ref() {
                    Item::Block(items) => {
                        for item in items {
                            writeln!(f, "\t{}", item)?;
                        }
                    }
                    _ => unimplemented!(),
                }
                write!(f, "}}")
            }
            Item::Block(items) => {
                writeln!(f, "{{")?;
                for item in items {
                    writeln!(f, "\t{}", item)?;
                }
                write!(f, "}}")
            }
            Item::Return(expr) => write!(f, "return {}", expr),
            Item::Decl(ident, ty, value) => write!(f, "let {}: {:?} = {};", ident, ty, value),
            Item::Assign(target, expr) => write!(f, "{} = {}", target, expr),
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
            Item::Unary(op, right) => write!(f, "({} {})", op, right),
        }
    }
}

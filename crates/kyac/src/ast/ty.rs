use crate::token::Token;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub base: Token,
    pub params: Vec<Type>,
}

impl Type {
    pub fn new(base: Token, params: Vec<Type>) -> Self {
        Self { base, params }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.base.lexeme.unwrap_or("no lexeme found"))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParameter {
    pub name: Token,
    pub bound: Option<Token>,
}

impl TypeParameter {
    pub fn new(name: Token, bound: Option<Token>) -> Self {
        Self { name, bound }
    }
}

use std::fmt;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum Kind {
    Identifier,
    Literal,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Colon,
    Comma,
    Dot,

    Plus,
    Minus,
    Star,
    Slash,

    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Let,
    Const,
    Fun,
    Return,
    Extern,
    If,
    Else,
    While,

    Rec,
    Init,

    Error,

    Eof,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Kind::Bang => write!(f, "!"),
            Kind::BangEqual => write!(f, "!="),
            Kind::Equal => write!(f, "="),
            Kind::EqualEqual => write!(f, "=="),
            Kind::Greater => write!(f, ">"),
            Kind::GreaterEqual => write!(f, ">="),
            Kind::Less => write!(f, "<"),
            Kind::LessEqual => write!(f, "<="),
            Kind::LeftParen => write!(f, "("),
            Kind::RightParen => write!(f, ")"),
            Kind::LeftBrace => write!(f, "{{"),
            Kind::RightBrace => write!(f, "}}"),
            Kind::Semicolon => write!(f, ";"),
            Kind::Colon => write!(f, ":"),
            Kind::Comma => write!(f, ","),
            Kind::Dot => write!(f, "."),
            Kind::Plus => write!(f, "add"),
            Kind::Minus => write!(f, "subtract"),
            Kind::Star => write!(f, "multiply"),
            Kind::Slash => write!(f, "divide"),
            Kind::Let => write!(f, "let"),
            Kind::Const => write!(f, "const"),
            Kind::Fun => write!(f, "fun"),
            Kind::Extern => write!(f, "extern"),
            Kind::Return => write!(f, "return"),
            Kind::Rec => write!(f, "rec"),
            Kind::Init => write!(f, "init"),
            Kind::If => write!(f, "if"),
            Kind::Else => write!(f, "else"),
            Kind::While => write!(f, "while"),
            Kind::Identifier => write!(f, "identifier"),
            Kind::Literal => write!(f, "literal"),
            Kind::Error => write!(f, "error"),
            Kind::Eof => write!(f, "eof"),
        }
    }
}

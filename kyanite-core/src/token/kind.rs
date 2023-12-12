use std::fmt;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum TokenKind {
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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Plus => write!(f, "add"),
            TokenKind::Minus => write!(f, "subtract"),
            TokenKind::Star => write!(f, "multiply"),
            TokenKind::Slash => write!(f, "divide"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Const => write!(f, "const"),
            TokenKind::Fun => write!(f, "fun"),
            TokenKind::Extern => write!(f, "extern"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Rec => write!(f, "rec"),
            TokenKind::Init => write!(f, "init"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Literal => write!(f, "literal"),
            TokenKind::Error => write!(f, "error"),
            TokenKind::Eof => write!(f, "eof"),
        }
    }
}

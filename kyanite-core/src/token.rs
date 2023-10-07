use serde::{Deserialize, Serialize};
use std::{fmt, hash::Hash, io};

use crate::{reporting::error::PreciseError, Source};

#[derive(Debug, PartialEq, Eq, Copy, Clone, Serialize, Deserialize, Hash)]
pub enum TokenKind {
    Identifier,
    Type,
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
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Type => write!(f, "type"),
            TokenKind::Literal => write!(f, "literal"),
            TokenKind::Error => write!(f, "error"),
            TokenKind::Eof => write!(f, "eof"),
        }
    }
}

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: Option<String>,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, lexeme: Option<String>, span: Span) -> Self {
        Self { kind, lexeme, span }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.lexeme == other.lexeme
    }
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state)
    }
}

impl From<&Token> for String {
    fn from(token: &Token) -> Self {
        token.lexeme.clone().unwrap_or(format!("{}", token.kind))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.lexeme.as_ref().unwrap_or(&format!("{}", self.kind))
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Span {
    pub(super) line: usize,
    pub(super) column: usize,
    pub(super) length: usize,
}

impl Span {
    pub fn new(line: usize, column: usize, length: usize) -> Self {
        Self {
            line,
            column,
            length,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
            length: 0,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{} to {}:{}",
            self.line,
            self.column,
            self.line,
            self.column + self.length
        )
    }
}

#[derive(Debug, Default)]
pub struct TokenStream {
    pub(super) errors: Vec<PreciseError>,
    pub(super) tokens: Vec<Token>,
    pub(super) source: Source,
    span: Span,
    start: usize,
    current: usize,
}

impl From<Source> for TokenStream {
    fn from(source: Source) -> Self {
        Self {
            source,
            ..Default::default()
        }
    }
}

impl TokenStream {
    pub fn from_source(source: Source) -> Result<Self, io::Error> {
        let mut stream = Self::from(source);
        stream.process();
        Ok(stream)
    }

    pub fn from_string(source: String) -> Result<Self, io::Error> {
        Self::from_source(Source {
            chars: source.chars().collect(),
            ..Default::default()
        })
    }

    fn process(&mut self) {
        while !self.eof() {
            self.token();
        }
    }

    fn token(&mut self) {
        self.span.length = 1;
        self.skip_whitespace();
        let token: Option<char> = self.peek();
        let token = match token {
            Some(token) => {
                self.consume();
                match token {
                    'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
                    // Punctuation
                    '(' => Token::new(TokenKind::LeftParen, None, self.span),
                    ')' => Token::new(TokenKind::RightParen, None, self.span),
                    ';' => Token::new(TokenKind::Semicolon, None, self.span),
                    ',' => Token::new(TokenKind::Comma, None, self.span),
                    ':' => Token::new(TokenKind::Colon, None, self.span),
                    '.' => Token::new(TokenKind::Dot, None, self.span),
                    '{' => Token::new(TokenKind::LeftBrace, None, self.span),
                    '}' => Token::new(TokenKind::RightBrace, None, self.span),
                    // Types
                    '"' => self.string(),
                    '0'..='9' => self.number(),
                    // Math
                    '+' => Token::new(TokenKind::Plus, None, self.span),
                    '-' => Token::new(TokenKind::Minus, None, self.span),
                    '*' => Token::new(TokenKind::Star, None, self.span),
                    '/' => Token::new(TokenKind::Slash, None, self.span),
                    // Logic
                    '=' => self.match_next('=', TokenKind::EqualEqual, TokenKind::Equal),
                    '!' => self.match_next('=', TokenKind::BangEqual, TokenKind::Bang),
                    '<' => self.match_next('=', TokenKind::LessEqual, TokenKind::Less),
                    '>' => self.match_next('=', TokenKind::GreaterEqual, TokenKind::Greater),
                    '%' => {
                        while !self.eof() && self.peek().unwrap() != '\n' {
                            self.consume();
                        }
                        return;
                    }
                    c => {
                        let error = PreciseError::new(
                            &self.source,
                            self.span,
                            format!("unexpected character `{c}`"),
                            "not a token".into(),
                        );
                        println!("{}", error);
                        self.errors.push(error);
                        Token::new(TokenKind::Error, None, self.span)
                    }
                }
            }
            None => Token::new(TokenKind::Eof, None, self.span),
        };
        self.tokens.push(token);
    }

    fn peek(&self) -> Option<char> {
        self.source.chars.get(self.current).copied()
    }

    fn string(&mut self) -> Token {
        self.start = self.current;
        let oquote = self.span;
        let mut peeked = self.peek();
        while !self.eof() && peeked.unwrap() != '"' {
            self.consume();
            peeked = self.peek();
        }
        self.consume(); // don't forget the closing quote
        self.span.length = self.current - self.start + 1;
        if self.eof() {
            let error = PreciseError::new(
                &self.source,
                oquote,
                "unterminated string".into(),
                "opening quote here".into(),
            );
            println!("{}", error);
            self.errors.push(error);
            return Token::new(TokenKind::Error, None, self.span);
        }
        let lexeme = self.lexeme(self.start - 1, self.current);
        self.adjusted(|stream: &TokenStream| {
            Token::new(TokenKind::Literal, Some(lexeme), stream.span)
        })
    }

    fn number(&mut self) -> Token {
        self.start = self.current - 1;
        let mut peeked = self.peek();
        while !self.eof() && self.num(peeked.unwrap()) {
            self.consume();
            peeked = self.peek();
        }

        if !self.eof() && self.peek().unwrap() == '.' {
            self.consume();
            peeked = self.peek();
            while !self.eof() && self.num(peeked.unwrap()) {
                self.consume();
                peeked = self.peek();
            }
        }

        let lexeme = self.lexeme(self.start, self.current);
        self.span.length = self.current - self.start;
        self.adjusted(|stream| Token::new(TokenKind::Literal, Some(lexeme), stream.span))
    }

    fn identifier(&mut self) -> Token {
        self.start = self.current - 1;
        let mut peeked = self.peek();
        while !self.eof() && self.ident(peeked.unwrap()) {
            self.consume();
            peeked = self.peek();
        }
        self.keyword(self.lexeme(self.start, self.current))
    }

    fn keyword(&mut self, lexeme: String) -> Token {
        self.span.length = self.current - self.start;
        self.adjusted(|stream| match lexeme.as_str() {
            "let" => Token::new(TokenKind::Let, None, stream.span),
            "const" => Token::new(TokenKind::Const, None, stream.span),
            "fun" => Token::new(TokenKind::Fun, None, stream.span),
            "str" | "float" | "int" | "void" | "bool" => {
                Token::new(TokenKind::Type, Some(lexeme), stream.span)
            }
            "true" => Token::new(TokenKind::Literal, Some(lexeme), stream.span),
            "false" => Token::new(TokenKind::Literal, Some(lexeme), stream.span),
            "return" => Token::new(TokenKind::Return, None, stream.span),
            "extern" => Token::new(TokenKind::Extern, None, stream.span),
            _ => Token::new(TokenKind::Identifier, Some(lexeme), stream.span),
        })
    }

    fn adjusted<F>(&mut self, f: F) -> Token
    where
        F: FnOnce(&Self) -> Token,
    {
        let prev = self.span.column;
        self.span.column = self.span.column - self.span.length + 1;
        let token = f(self);
        self.span.column = prev;
        token
    }

    fn lexeme(&self, start: usize, end: usize) -> String {
        self.source.chars[start..end].iter().collect()
    }

    fn match_next(&mut self, c: char, first: TokenKind, second: TokenKind) -> Token {
        if self.peek().unwrap() == c {
            self.consume();
            self.span.length += 1;
            Token::new(first, None, self.span)
        } else {
            Token::new(second, None, self.span)
        }
    }

    fn ident(&self, c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn num(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn whitespace(&self, c: char) -> bool {
        matches!(c, '\t' | '\n' | ' ')
    }

    fn skip_whitespace(&mut self) {
        let mut peeked = self.peek();
        while !self.eof() && self.whitespace(peeked.unwrap()) {
            self.consume();
            if peeked.unwrap() == '\n' {
                self.span.line += 1;
                self.span.column = 0;
            }
            peeked = self.peek();
        }
    }

    fn consume(&mut self) {
        self.current += 1;
        self.span.column += 1;
    }

    fn eof(&self) -> bool {
        self.peek().is_none()
    }
}

macro_rules! assert_tokens {
    ($($path:expr => $name:ident / $valid:expr),*) => {
        #[cfg(test)]
        mod tests {
            use crate::token::{TokenStream, Source};

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let stream = TokenStream::from_source(Source::new($path)?)?;
                    insta::with_settings!({snapshot_path => "../snapshots"}, {
                        if $valid {
                            insta::assert_yaml_snapshot!(stream.tokens);
                        } else {
                            insta::assert_yaml_snapshot!(stream.errors);
                        }
                    });

                    Ok(())
                }
            )*
        }
    };
}

assert_tokens! {
    "test-cases/hello.kya" => hello_world / true,
    "test-cases/expr.kya" => expr / true,
    "test-cases/calls.kya" => calls / true,
    "test-cases/empty.kya" => empty / true,
    "test-cases/access.kya" => access / true,
    "test-cases/mixed.kya" => mixed / true,

    "test-cases/tokens/varied.kya" => varied / false,
    "test-cases/tokens/loooong.kya" => loooong / false,
    "test-cases/tokens/unterm.kya" => unterm / false
}

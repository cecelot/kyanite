use std::{
    fs::File,
    io::{self, Read},
};

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    Literal,
    // Punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Colon,
    Comma,
    // Types
    String,
    Float,
    Int,
    Void,
    Bool,
    // Math
    Plus,
    Minus,
    Star,
    Slash,
    // Logic
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Keywords
    Let,
    Defn,
    Return,
    // Error
    Error,

    Eof,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    lexeme: Option<String>,
    span: Span,
}

impl Token {
    fn new(kind: TokenKind, lexeme: Option<String>, span: Span) -> Self {
        Self { kind, lexeme, span }
    }
}

#[derive(Debug, Copy, Clone)]
struct Span {
    line: usize,
    column: usize,
}

pub fn errored(tokens: &Vec<Token>) -> bool {
    tokens.iter().any(|t| t.kind == TokenKind::Error)
}

#[derive(Debug)]
pub struct TokenStream {
    source: Vec<char>,
    span: Span,
    start: usize,
    current: usize,
}

impl TokenStream {
    pub fn new(mut file: File) -> Result<Self, io::Error> {
        let mut source: String = "".into();
        file.read_to_string(&mut source)?;
        Ok(Self {
            source: source.chars().collect::<Vec<char>>(),
            span: Span { line: 1, column: 0 },
            start: 0,
            current: 0,
        })
    }

    fn token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let token: Option<char> = self.peek();
        Some(match token {
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
                    _ => {
                        eprintln!(
                            "{}:{}: Unexpected character: {}",
                            self.span.line,
                            self.span.column - 1,
                            token
                        );
                        Token::new(TokenKind::Error, None, self.span)
                    }
                }
            }
            None => Token::new(TokenKind::Eof, None, self.span),
        })
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.current).copied()
    }

    fn string(&mut self) -> Token {
        self.start = self.current;
        let mut peeked = self.peek();
        while !self.eof() && peeked.unwrap() != '"' {
            self.consume();
            peeked = self.peek();
        }
        self.consume(); // don't forget the closing quote
        let lexeme = self.lexeme(self.current - 1);
        Token {
            span: self.span,
            kind: TokenKind::Literal,
            lexeme: Some(lexeme),
        }
    }

    fn number(&mut self) -> Token {
        self.start = self.current - 1;
        let mut peeked = self.peek();
        while !self.eof() && self.num(peeked.unwrap()) {
            self.consume();
            peeked = self.peek();
        }

        // if we have a decimal point, we have a float
        if self.peek().unwrap() == '.' {
            self.consume(); // consume '.'
            peeked = self.peek();
            while !self.eof() && self.num(peeked.unwrap()) {
                self.consume();
                peeked = self.peek();
            }
        }

        let lexeme = self.lexeme(self.current);
        Token {
            span: self.span,
            kind: TokenKind::Literal,
            lexeme: Some(lexeme),
        }
    }

    fn identifier(&mut self) -> Token {
        self.start = self.current - 1;
        let mut peeked = self.peek();
        while !self.eof() && self.ident(peeked.unwrap()) {
            self.consume();
            peeked = self.peek();
        }
        self.keyword(self.lexeme(self.current))
    }

    fn keyword(&mut self, lexeme: String) -> Token {
        match lexeme.as_str() {
            "let" => Token::new(TokenKind::Let, None, self.span),
            "defn" => Token::new(TokenKind::Defn, None, self.span),
            "string" => Token::new(TokenKind::String, None, self.span),
            "float" => Token::new(TokenKind::Float, None, self.span),
            "int" => Token::new(TokenKind::Int, None, self.span),
            "void" => Token::new(TokenKind::Void, None, self.span),
            "bool" => Token::new(TokenKind::Bool, None, self.span),
            "return" => Token::new(TokenKind::Return, None, self.span),
            _ => Token::new(TokenKind::Identifier, Some(lexeme), self.span),
        }
    }

    fn lexeme(&self, end: usize) -> String {
        self.source[self.start..end].iter().collect::<String>()
    }

    fn match_next(&mut self, c: char, first: TokenKind, second: TokenKind) -> Token {
        if self.peek().unwrap() == c {
            self.consume();
            Token::new(first, None, self.span)
        } else {
            Token::new(second, None, self.span)
        }
    }

    fn ident(&self, c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn num(&self, c: char) -> bool {
        matches!(c, '0'..='9')
    }

    fn whitespace(&self, c: char) -> bool {
        matches!(c, '\t' | '\n' | ' ')
    }

    fn skip_whitespace(&mut self) {
        let mut peeked = self.peek();
        while !self.eof() && self.whitespace(peeked.unwrap()) {
            if peeked.unwrap() == '\n' {
                self.span.line += 1;
                self.span.column = 0;
            }
            self.consume();
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

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.eof() {
            self.token()
        } else {
            None
        }
    }
}

use std::{
    collections::VecDeque,
    fmt,
    hash::{Hash, Hasher},
};

use crate::{reporting::error::PreciseError, Source};

pub use self::kind::Kind;

mod kind;

#[derive(Debug, Clone, Eq)]
pub struct Token {
    pub kind: Kind,
    pub lexeme: Option<&'static str>,
    pub span: Span,
}

impl Token {
    pub fn new(kind: Kind, lexeme: Option<&'static str>, span: Span) -> Self {
        Self { kind, lexeme, span }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.lexeme == other.lexeme
    }
}

impl PartialEq<str> for Token {
    fn eq(&self, other: &str) -> bool {
        self.lexeme == Some(other)
    }
}

impl PartialEq<&str> for Token {
    fn eq(&self, other: &&str) -> bool {
        self.lexeme == Some(other)
    }
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state);
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.lexeme.unwrap_or(&format!("{}", self.kind)))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug)]
pub struct TokenStream<'a> {
    pub(super) errors: Vec<PreciseError<'a>>,
    pub(super) tokens: VecDeque<Token>,
    pub(super) source: &'a Source,
    span: Span,
    start: usize,
    current: usize,
}

impl<'a> From<&'a Source> for TokenStream<'a> {
    fn from(source: &'a Source) -> Self {
        Self {
            source,
            errors: vec![],
            tokens: VecDeque::new(),
            span: Span::default(),
            start: 0,
            current: 0,
        }
    }
}

impl<'a> TokenStream<'a> {
    pub fn from_source(source: &'a Source) -> Self {
        let mut stream = Self::from(source);
        stream.process();
        stream
    }

    fn process(&mut self) {
        while !self.eof() {
            self.token();
        }
    }

    fn token(&mut self) {
        self.span.length = 1;
        self.skip_whitespace();
        let token = match self.peek() {
            Some(token) => {
                self.consume();
                match token {
                    'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
                    // Punctuation
                    '(' => Token::new(Kind::LeftParen, None, self.span),
                    ')' => Token::new(Kind::RightParen, None, self.span),
                    ';' => Token::new(Kind::Semicolon, None, self.span),
                    ',' => Token::new(Kind::Comma, None, self.span),
                    ':' => Token::new(Kind::Colon, None, self.span),
                    '.' => Token::new(Kind::Dot, None, self.span),
                    '{' => Token::new(Kind::LeftBrace, None, self.span),
                    '}' => Token::new(Kind::RightBrace, None, self.span),
                    // Types
                    '"' => self.string(),
                    '0'..='9' => self.number(),
                    // Math
                    '+' => Token::new(Kind::Plus, None, self.span),
                    '-' => Token::new(Kind::Minus, None, self.span),
                    '*' => Token::new(Kind::Star, None, self.span),
                    '/' => Token::new(Kind::Slash, None, self.span),
                    // Logic
                    '=' => self.match_next('=', Kind::EqualEqual, Kind::Equal),
                    '!' => self.match_next('=', Kind::BangEqual, Kind::Bang),
                    '<' => self.match_next('=', Kind::LessEqual, Kind::Less),
                    '>' => self.match_next('=', Kind::GreaterEqual, Kind::Greater),
                    '%' => {
                        while !self.eof() && self.peek().unwrap() != '\n' {
                            self.consume();
                        }
                        return;
                    }
                    c => {
                        let error = PreciseError::new(
                            self.source,
                            self.span,
                            format!("unexpected character `{c}`"),
                            "not a token".into(),
                        );
                        println!("{error}");
                        self.errors.push(error);
                        Token::new(Kind::Error, None, self.span)
                    }
                }
            }
            None => Token::new(Kind::Eof, None, self.span),
        };
        self.tokens.push_back(token);
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
                self.source,
                oquote,
                "unterminated string".into(),
                "opening quote here".into(),
            );
            println!("{error}");
            self.errors.push(error);
            return Token::new(Kind::Error, None, self.span);
        }
        let lexeme = self.lexeme(self.start - 1, self.current);
        self.adjusted(|stream| Token::new(Kind::Literal, Some(lexeme.leak()), stream.span))
    }

    fn number(&mut self) -> Token {
        self.start = self.current - 1;
        let mut peeked = self.peek();
        while !self.eof() && Self::num(peeked.unwrap()) {
            self.consume();
            peeked = self.peek();
        }

        if !self.eof() && self.peek().unwrap() == '.' {
            self.consume();
            peeked = self.peek();
            while !self.eof() && Self::num(peeked.unwrap()) {
                self.consume();
                peeked = self.peek();
            }
        }

        let lexeme = self.lexeme(self.start, self.current);
        self.span.length = self.current - self.start;
        self.adjusted(|stream| Token::new(Kind::Literal, Some(lexeme.leak()), stream.span))
    }

    fn identifier(&mut self) -> Token {
        self.start = self.current - 1;
        let mut peeked = self.peek();
        while !self.eof() && Self::ident(peeked.unwrap()) {
            self.consume();
            peeked = self.peek();
        }
        self.keyword(self.lexeme(self.start, self.current))
    }

    fn keyword(&mut self, lexeme: String) -> Token {
        self.span.length = self.current - self.start;
        self.adjusted(|stream| match lexeme.as_str() {
            "let" => Token::new(Kind::Let, None, stream.span),
            "const" => Token::new(Kind::Const, None, stream.span),
            "fun" => Token::new(Kind::Fun, None, stream.span),
            "true" | "false" => Token::new(Kind::Literal, Some(lexeme.leak()), stream.span),
            "return" => Token::new(Kind::Return, None, stream.span),
            "extern" => Token::new(Kind::Extern, None, stream.span),
            "rec" => Token::new(Kind::Rec, None, stream.span),
            "init" => Token::new(Kind::Init, None, stream.span),
            "if" => Token::new(Kind::If, None, stream.span),
            "else" => Token::new(Kind::Else, None, stream.span),
            "while" => Token::new(Kind::While, None, stream.span),
            _ => Token::new(Kind::Identifier, Some(lexeme.leak()), stream.span),
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

    fn match_next(&mut self, c: char, first: Kind, second: Kind) -> Token {
        if self.peek().unwrap() == c {
            self.consume();
            self.span.length = 2;
            let mut span = self.span;
            span.column -= 1;
            Token::new(first, None, span)
        } else {
            Token::new(second, None, self.span)
        }
    }

    fn ident(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn num(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn whitespace(c: char) -> bool {
        matches!(c, '\t' | '\n' | ' ')
    }

    fn skip_whitespace(&mut self) {
        let mut peeked = self.peek();
        while !self.eof() && Self::whitespace(peeked.unwrap()) {
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
                    let source = Source::new($path)?;
                    let stream = TokenStream::from_source(&source);
                    insta::with_settings!({snapshot_path => "../../snapshots"}, {
                        if $valid {
                            insta::assert_debug_snapshot!(stream.tokens);
                        } else {
                            insta::assert_debug_snapshot!(stream.errors);
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

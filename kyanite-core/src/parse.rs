use std::collections::VecDeque;

use crate::{
    ast::{init, Decl, Expr, Field, Initializer, Param, Stmt},
    reporting::error::PreciseError,
    token::{Span, Token, TokenKind},
    Source,
};

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("unexpected EOF")]
    UnexpectedEof(Span),
    #[error("expected {0} but found {2}")]
    Expected(TokenKind, Span, TokenKind),
    #[error("unexpected {0}")]
    Unhandled(TokenKind, Span, &'static [TokenKind]),
}

pub struct Parser<'a> {
    pub(super) errors: Vec<PreciseError>,
    source: &'a Source,
    tokens: VecDeque<Token>,
    previous: Option<Token>,
    panic: bool,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a Source, tokens: VecDeque<Token>) -> Self {
        Self {
            source,
            tokens,
            panic: false,
            errors: vec![],
            previous: None,
        }
    }

    pub fn parse(&mut self) -> Vec<Decl> {
        let mut nodes: Vec<Decl> = vec![];
        while let Ok(token) = self.peek() {
            match match token.kind {
                TokenKind::Rec => self.record(),
                TokenKind::Fun => self.function(false),
                TokenKind::Extern => self.function(true),
                TokenKind::Const => self.constant(),
                TokenKind::Eof => break,
                _ => {
                    let token = self.advance().unwrap();
                    Err(ParseError::Unhandled(
                        token.kind,
                        token.span,
                        &[TokenKind::Fun, TokenKind::Const],
                    ))
                }
            } {
                Ok(node) => nodes.push(node),
                Err(e) => {
                    self.error(&e);
                    self.synchronize(false);
                }
            }
        }
        nodes
    }

    fn record(&mut self) -> Result<Decl, ParseError> {
        self.consume(TokenKind::Rec)?;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::LeftBrace)?;
        let fields = self.fields()?;
        self.consume(TokenKind::RightBrace)?;
        Ok(init::record(name, fields))
    }

    fn function(&mut self, external: bool) -> Result<Decl, ParseError> {
        if external {
            self.consume(TokenKind::Extern)?;
        }

        self.consume(TokenKind::Fun)?;
        let name = self.consume(TokenKind::Identifier)?;

        self.consume(TokenKind::LeftParen)?;
        let params = self.params()?;
        self.consume(TokenKind::RightParen)?;

        let mut ty: Option<Token> = None;
        if self.peek()?.kind == TokenKind::Colon {
            self.consume(TokenKind::Colon)?;
            ty = Some(self.consume(TokenKind::Identifier)?);
        }

        if external {
            Ok(init::func(name, params, ty, vec![], external))
        } else {
            Ok(init::func(name, params, ty, self.block()?, external))
        }
    }

    fn params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params: Vec<Param> = vec![];
        while self.peek()?.kind != TokenKind::RightParen {
            let name = self.consume(TokenKind::Identifier)?;
            self.consume(TokenKind::Colon)?;
            let ty = self.consume(TokenKind::Identifier)?;
            params.push(Param::new(name, ty));
            if self.peek()?.kind != TokenKind::RightParen {
                self.consume(TokenKind::Comma)?;
            }
        }
        Ok(params)
    }

    fn fields(&mut self) -> Result<Vec<Field>, ParseError> {
        let mut fields: Vec<Field> = vec![];
        while self.peek()?.kind != TokenKind::RightBrace {
            let name = self.consume(TokenKind::Identifier)?;
            self.consume(TokenKind::Colon)?;
            let ty = self.consume(TokenKind::Identifier)?;
            fields.push(Field::new(name, ty));
            if self.peek()?.kind != TokenKind::RightBrace {
                self.consume(TokenKind::Comma)?;
            }
        }
        Ok(fields)
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.consume(TokenKind::LeftBrace)?;
        let mut stmts: Vec<Stmt> = vec![];
        while self.peek()?.kind != TokenKind::RightBrace {
            let stmt = self.statement();
            match stmt {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.error(&e);
                    self.synchronize(true);
                }
            }
        }
        self.consume(TokenKind::RightBrace)?;
        Ok(stmts)
    }

    fn constant(&mut self) -> Result<Decl, ParseError> {
        self.consume(TokenKind::Const)?;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Equal)?;
        let value = self.expression()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(init::constant(name, ty, value))
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenKind::Let)?;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let ty = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Equal)?;
        let value = self.expression()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(init::var(name, ty, value))
    }

    fn condition(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenKind::If)?;
        let condition = self.expression()?;
        let is = self.block()?;
        if self.peek()?.kind == TokenKind::Else {
            self.consume(TokenKind::Else)?;
            let otherwise = self.block()?;
            Ok(init::conditional(condition, is, otherwise))
        } else {
            Ok(init::conditional(condition, is, vec![]))
        }
    }

    fn loops(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenKind::While)?;
        let condition = self.expression()?;
        let block = self.block()?;
        Ok(init::loops(condition, block))
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek()?.kind {
            TokenKind::Let => self.declaration(),
            TokenKind::If => self.condition(),
            TokenKind::While => self.loops(),
            TokenKind::Return => {
                let token = self.consume(TokenKind::Return)?;
                let value = self.expression()?;
                self.consume(TokenKind::Semicolon)?;
                Ok(init::ret(value, token))
            }
            _ => self.assignment(),
        }
    }

    fn assignment(&mut self) -> Result<Stmt, ParseError> {
        let item = self.expression()?;
        if self.peek()?.kind == TokenKind::Equal {
            self.consume(TokenKind::Equal)?;
            let right = self.expression()?;
            self.consume(TokenKind::Semicolon)?;
            Ok(init::assign(item, right))
        } else {
            self.consume(TokenKind::Semicolon)?;
            Ok(Stmt::Expr(item))
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while matches!(
            self.peek()?.kind,
            TokenKind::BangEqual | TokenKind::EqualEqual
        ) {
            let operator = self.advance().unwrap();
            let right = self.comparison()?;
            expr = init::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while matches!(
            self.peek()?.kind,
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        ) {
            let operator = self.advance().unwrap();
            let right = self.term()?;
            expr = init::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while matches!(self.peek()?.kind, TokenKind::Minus | TokenKind::Plus) {
            let operator = self.advance().unwrap();
            let right = self.factor()?;
            expr = init::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while matches!(self.peek()?.kind, TokenKind::Slash | TokenKind::Star) {
            let operator = self.advance().unwrap();
            let right = self.unary()?;
            expr = init::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        Ok(match self.peek()?.kind {
            TokenKind::Bang | TokenKind::Minus => {
                let operator = self.advance().unwrap();
                let right = self.unary()?;
                init::unary(operator, right)
            }
            _ => self.access()?,
        })
    }

    fn access(&mut self) -> Result<Expr, ParseError> {
        let item = self.call()?;
        let mut chain: Vec<Expr> = vec![];

        while self.peek()?.kind == TokenKind::Dot {
            self.consume(TokenKind::Dot)?;
            chain.push(self.call()?);
            if self.peek()?.kind != TokenKind::Dot {
                chain.insert(0, item);
                return Ok(init::access(chain));
            }
        }

        Ok(item)
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.primary()?;
        if self.peek()?.kind == TokenKind::LeftParen {
            let open = self.consume(TokenKind::LeftParen)?;
            let mut args: Vec<Expr> = vec![];
            let mut delimiters: Vec<Token> = vec![];
            if self.peek()?.kind != TokenKind::RightParen {
                args.push(self.expression()?);
                while self.peek()?.kind == TokenKind::Comma {
                    delimiters.push(self.consume(TokenKind::Comma)?);
                    args.push(self.expression()?);
                }
            }
            let close = self.consume(TokenKind::RightParen)?;
            left = init::call(left, args, (open, close), delimiters);
        }
        Ok(left)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        Ok(match self.peek()?.kind {
            TokenKind::LeftParen => {
                self.consume(TokenKind::LeftParen)?;
                let expr = self.expression()?;
                self.consume(TokenKind::RightParen)?;
                expr
            }
            TokenKind::Literal => {
                let token = self.advance().unwrap();
                let lexeme = token.lexeme.as_ref().unwrap();
                match &lexeme[..] {
                    "true" | "false" => Expr::Bool(lexeme == "true", token),
                    _ if lexeme.starts_with('"') => Expr::Str(lexeme.clone(), token),
                    _ if lexeme.contains('.') => Expr::Float(lexeme.parse().unwrap(), token),
                    _ if lexeme.chars().next().unwrap().is_ascii_digit() => {
                        Expr::Int(lexeme.parse().unwrap(), token)
                    }
                    e => unreachable!("impossible lexeme `{}`", e),
                }
            }
            TokenKind::Identifier => {
                let name = self.advance().unwrap();
                if self.peek()?.kind == TokenKind::Colon {
                    self.init(name)?
                } else {
                    init::ident(name)
                }
            }
            _ => Err(ParseError::Unhandled(
                self.peek()?.kind,
                self.peek()?.span,
                &[
                    TokenKind::Identifier,
                    TokenKind::Literal,
                    TokenKind::LeftParen,
                ],
            ))?,
        })
    }

    fn init(&mut self, name: Token) -> Result<Expr, ParseError> {
        self.consume(TokenKind::Colon)?;
        self.consume(TokenKind::Init)?;
        let left = self.consume(TokenKind::LeftParen)?;
        let mut initializers: Vec<Initializer> = vec![];
        while self.peek()?.kind != TokenKind::RightParen {
            let name = self.consume(TokenKind::Identifier)?;
            self.consume(TokenKind::Colon)?;
            let value = self.expression()?;
            initializers.push(Initializer::new(name, value));
            if self.peek()?.kind != TokenKind::RightParen {
                self.consume(TokenKind::Comma)?;
            }
        }
        let right = self.consume(TokenKind::RightParen)?;
        Ok(init::init(name, initializers, (left, right)))
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.eof() {
            return Err(ParseError::Expected(
                kind,
                self.previous.as_ref().unwrap().span,
                TokenKind::Eof,
            ));
        }

        let token = self.advance().unwrap();
        if token.kind == kind {
            Ok(token)
        } else {
            Err(ParseError::Expected(kind, token.span, token.kind))
        }
    }

    fn eof(&self) -> bool {
        self.tokens.is_empty()
    }

    fn peek(&self) -> Result<&Token, ParseError> {
        let span = if self.previous.is_some() {
            if self.eof() {
                return Err(ParseError::UnexpectedEof(Span::new(0, 0, 0)));
            }
            self.previous.as_ref().unwrap().span
        } else {
            Span::new(0, 0, 0)
        };
        match self.tokens.front() {
            Some(token) => Ok(token),
            None => Err(ParseError::UnexpectedEof(span)),
        }
    }

    fn error(&mut self, e: &ParseError) {
        self.panic = true;
        let span = *match &e {
            ParseError::Expected(_, span, _) => span,
            ParseError::Unhandled(_, span, _) => span,
            ParseError::UnexpectedEof(span) => span,
        };
        let detail = match e {
            ParseError::Expected(expected, _, _) => format!("expected {} here", expected),
            ParseError::Unhandled(_, _, expected) => {
                let expected = expected
                    .iter()
                    .map(|t| format!("`{}`", t))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("expected one of {} here", expected)
            }
            ParseError::UnexpectedEof(_) => "unexpected end of file".into(),
        };
        let error = PreciseError::new(self.source, span, format!("{}", e), detail);
        println!("{}", error);
        self.errors.push(error);
    }

    fn synchronize(&mut self, stmt: bool) {
        self.panic = false;

        if stmt {
            self.advance();
        }

        while !self.eof() {
            if self.previous.as_ref().unwrap().kind == TokenKind::Semicolon
                || self.previous.as_ref().unwrap().span.line < self.peek().unwrap().span.line
            {
                return;
            }

            if matches!(
                self.peek().unwrap().kind,
                TokenKind::Let | TokenKind::Fun | TokenKind::Const
            ) {
                return;
            }
            self.advance();
        }
    }

    fn advance(&mut self) -> Option<Token> {
        let previous = self.tokens.pop_front();
        self.previous = previous.clone();
        previous
    }
}

macro_rules! assert_parse {
    ($($path:expr => $name:ident / $valid:expr),*) => {
        #[cfg(test)]
        mod tests {
            use crate::{parse::Parser, token::{TokenStream}, Source};

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let source = Source::new($path)?;
                    let stream = TokenStream::from_source(&source)?;
                    let mut parser = Parser::new(stream.source, stream.tokens);
                    parser.parse();
                    insta::with_settings!({snapshot_path => "../snapshots"}, {
                        if $valid {
                            insta::assert_display_snapshot!(parser.errors.len());
                        } else {
                            insta::assert_yaml_snapshot!(parser.errors);
                        }
                    });

                    Ok(())
                }
            )*
        }
    };
}

assert_parse! {
    // Ensure valid programs have zero parse errors
    "test-cases/hello.kya" => hello_world / true,
    "test-cases/expr.kya" => expr / true,
    "test-cases/calls.kya" => calls / true,
    "test-cases/empty.kya" => empty / true,
    "test-cases/access.kya" => access / true,
    "test-cases/mixed.kya" => mixed / true,

    "test-cases/parser/simple.kya" => simple / false,
    "test-cases/parser/toplevel.kya" => toplevel / false,
    "test-cases/parser/nested.kya" => nested / false
}

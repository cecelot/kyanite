use crate::{
    ast::{File, Node, Param},
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

pub struct Parser {
    pub(super) errors: Vec<PreciseError>,
    source: Source,
    tokens: Vec<Token>,
    current: usize,
    panic: bool,
}

impl Parser {
    pub fn new(source: Source, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            panic: false,
            errors: vec![],
            current: 0,
        }
    }

    pub fn parse(&mut self) -> File {
        let mut nodes: Vec<Node> = vec![];
        while let Ok(token) = self.peek() {
            match match token.kind {
                TokenKind::Defn => self.function(),
                TokenKind::Const => self.constant(),
                TokenKind::Eof => break,
                _ => {
                    let token = self.advance().unwrap();
                    Err(ParseError::Unhandled(
                        token.kind,
                        token.span,
                        &[TokenKind::Defn, TokenKind::Const],
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
        let file = File::new(nodes);
        match file.nodes.first() {
            Some(_) => file,
            _ => File::new(vec![]),
        }
    }

    fn function(&mut self) -> Result<Node, ParseError> {
        self.consume(TokenKind::Defn)?;
        let name = self.consume(TokenKind::Identifier)?;

        self.consume(TokenKind::LeftParen)?;
        let params = self.params()?;
        self.consume(TokenKind::RightParen)?;

        let mut ty: Option<Token> = None;
        if self.peek()?.kind == TokenKind::Colon {
            self.consume(TokenKind::Colon)?;
            ty = Some(self.consume(TokenKind::Type)?);
        }

        self.consume(TokenKind::LeftBrace)?;
        let body = self.block()?;
        self.consume(TokenKind::RightBrace)?;

        Ok(Node::func(name, params, ty, body))
    }

    fn params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params: Vec<Param> = vec![];
        while self.peek()?.kind != TokenKind::RightParen {
            let name = self.consume(TokenKind::Identifier)?;
            self.consume(TokenKind::Colon)?;
            let ty = self.consume(TokenKind::Type)?;
            params.push(Param::new(name, ty));
            if self.peek()?.kind == TokenKind::Comma {
                self.consume(TokenKind::Comma)?;
            }
        }
        Ok(params)
    }

    fn block(&mut self) -> Result<Vec<Node>, ParseError> {
        let mut stmts: Vec<Node> = vec![];
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
        Ok(stmts)
    }

    fn constant(&mut self) -> Result<Node, ParseError> {
        self.consume(TokenKind::Const)?;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let ty = self.consume(TokenKind::Type)?;
        self.consume(TokenKind::Equal)?;
        let value = self.expression()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Node::constant(name, ty, value))
    }

    fn declaration(&mut self) -> Result<Node, ParseError> {
        self.consume(TokenKind::Let)?;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let ty = self.consume(TokenKind::Type)?;
        self.consume(TokenKind::Equal)?;
        let value = self.expression()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Node::var(name, ty, value))
    }

    fn statement(&mut self) -> Result<Node, ParseError> {
        match self.peek()?.kind {
            TokenKind::Let => self.declaration(),
            TokenKind::Return => {
                let token = self.consume(TokenKind::Return)?;
                let value = self.expression()?;
                self.consume(TokenKind::Semicolon)?;
                Ok(Node::ret(value, token))
            }
            _ => self.assignment(),
        }
    }

    fn assignment(&mut self) -> Result<Node, ParseError> {
        let mut item = self.expression()?;
        while self.peek()?.kind == TokenKind::Equal {
            self.consume(TokenKind::Equal)?;
            let right = self.expression()?;
            item = Node::assign(item, right);
        }
        self.consume(TokenKind::Semicolon)?;
        Ok(item)
    }

    fn expression(&mut self) -> Result<Node, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.comparison()?;

        while matches!(
            self.peek()?.kind,
            TokenKind::BangEqual | TokenKind::EqualEqual
        ) {
            let operator = self.advance().unwrap();
            let right = self.comparison()?;
            expr = Node::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.term()?;

        while matches!(
            self.peek()?.kind,
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        ) {
            let operator = self.advance().unwrap();
            let right = self.term()?;
            expr = Node::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.factor()?;

        while matches!(self.peek()?.kind, TokenKind::Minus | TokenKind::Plus) {
            let operator = self.advance().unwrap();
            let right = self.factor()?;
            expr = Node::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Node, ParseError> {
        let mut expr = self.unary()?;

        while matches!(self.peek()?.kind, TokenKind::Slash | TokenKind::Star) {
            let operator = self.advance().unwrap();
            let right = self.unary()?;
            expr = Node::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Node, ParseError> {
        Ok(match self.peek()?.kind {
            TokenKind::Bang | TokenKind::Minus => {
                let operator = self.advance().unwrap();
                let right = self.unary()?;
                Node::unary(operator, right)
            }
            _ => self.access()?,
        })
    }

    fn access(&mut self) -> Result<Node, ParseError> {
        let mut item = self.call()?;

        while self.peek()?.kind == TokenKind::Dot {
            let token = self.consume(TokenKind::Dot)?;
            let right = self.call()?;
            item = Node::binary(item, token, right);
        }

        Ok(item)
    }

    fn call(&mut self) -> Result<Node, ParseError> {
        let mut left = self.primary()?;
        if self.peek()?.kind == TokenKind::LeftParen {
            let open = self.consume(TokenKind::LeftParen)?;
            let mut args: Vec<Node> = vec![];
            let mut delimiters: Vec<Token> = vec![];
            if self.peek()?.kind != TokenKind::RightParen {
                args.push(self.expression()?);
                while self.peek()?.kind == TokenKind::Comma {
                    delimiters.push(self.consume(TokenKind::Comma)?);
                    args.push(self.expression()?);
                }
            }
            let close = self.consume(TokenKind::RightParen)?;
            left = Node::call(left, args, (open, close), delimiters);
        }
        Ok(left)
    }

    fn primary(&mut self) -> Result<Node, ParseError> {
        Ok(match self.peek()?.kind {
            TokenKind::LeftParen => {
                self.consume(TokenKind::LeftParen)?;
                let expr = self.expression()?;
                self.consume(TokenKind::RightParen)?;
                expr
            }
            TokenKind::Literal => {
                let token = self.advance().unwrap();
                let lexeme = token.lexeme.unwrap();
                match &lexeme[..] {
                    "true" | "false" => Node::Bool(lexeme == "true"),
                    _ if lexeme.starts_with('"') => Node::Str(lexeme),
                    _ if lexeme.contains('.') => Node::Float(lexeme.parse().unwrap()),
                    _ if lexeme.chars().next().unwrap().is_ascii_digit() => {
                        Node::Int(lexeme.parse().unwrap())
                    }
                    e => unreachable!("impossible lexeme `{}`", e),
                }
            }
            TokenKind::Identifier => {
                let token = self.advance().unwrap();
                Node::ident(token)
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

    fn consume(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.eof() {
            return Err(ParseError::Expected(
                kind,
                self.tokens[self.current - 1].span,
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
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Result<Token, ParseError> {
        let span = if self.current > 0 {
            if self.current > self.tokens.len() {
                return Err(ParseError::UnexpectedEof(Span::new(0, 0, 0)));
            }
            self.tokens[self.current - 1].span
        } else {
            Span::new(0, 0, 0)
        };
        match self.tokens.get(self.current).cloned() {
            Some(token) => Ok(token),
            None => Err(ParseError::UnexpectedEof(span)),
        }
    }

    fn previous(&self) -> Option<Token> {
        self.tokens.get(self.current - 1).cloned()
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
        let error = PreciseError::new(&self.source, span, format!("{}", e), detail);
        println!("{}", error);
        self.errors.push(error);
    }

    fn synchronize(&mut self, stmt: bool) {
        self.panic = false;

        if stmt {
            self.advance();
        }

        while !self.eof() {
            if self.previous().unwrap().kind == TokenKind::Semicolon
                || self.previous().unwrap().span.line < self.peek().unwrap().span.line
            {
                return;
            }

            if matches!(
                self.peek().unwrap().kind,
                TokenKind::Let | TokenKind::Defn | TokenKind::Const
            ) {
                return;
            }
            self.advance();
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.current += 1;
        self.tokens.get(self.current - 1).cloned()
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
                    let stream = TokenStream::new(Source::new($path)?)?;
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

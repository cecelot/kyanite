use crate::{
    ast::{Item, Param, Type},
    token::{Span, Token, TokenKind},
};

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("Unexpected EOF at {0}")]
    UnexpectedEof(Span),

    #[error("Expected {0} at {1}")]
    Expected(TokenKind, Span),

    #[error("Unhandled token {0} at {1}")]
    Unhandled(TokenKind, Span),
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Item, ParseError> {
        let mut items: Vec<Item> = vec![];
        while let Ok(token) = self.peek() {
            items.push(match token.kind {
                TokenKind::Defn => self.function()?,
                TokenKind::Eof => break,
                _ => self.statement()?,
            });
        }
        Ok(Item::Program(items))
    }

    fn function(&mut self) -> Result<Item, ParseError> {
        self.consume(TokenKind::Defn)?;
        let name = self.consume(TokenKind::Identifier)?;

        self.consume(TokenKind::LeftParen)?;
        let params = self.params()?;
        self.consume(TokenKind::RightParen)?;

        let mut ty = Type::Void;
        if self.peek()?.kind == TokenKind::Colon {
            self.consume(TokenKind::Colon)?;
            ty = Type::from(self.consume(TokenKind::Type)?.lexeme.unwrap());
        }

        self.consume(TokenKind::LeftBrace)?;
        let body = self.block()?;
        self.consume(TokenKind::RightBrace)?;
        Ok(Item::Function(
            name.lexeme.unwrap(),
            params,
            ty,
            Box::new(body),
        ))
    }

    fn params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params: Vec<Param> = vec![];
        while self.peek()?.kind != TokenKind::RightParen {
            let name = self.consume(TokenKind::Identifier)?.lexeme.unwrap();
            self.consume(TokenKind::Colon)?;
            let ty = self.consume(TokenKind::Type)?.lexeme.unwrap();
            params.push(Param::new(
                name,
                match ty.as_str() {
                    "str" => Type::Str,
                    "int" => Type::Int,
                    "float" => Type::Float,
                    "bool" => Type::Bool,
                    "void" => Type::Void,
                    _ => unimplemented!(),
                },
            ));
            if self.peek()?.kind == TokenKind::Comma {
                self.consume(TokenKind::Comma)?;
            }
        }
        Ok(params)
    }

    fn block(&mut self) -> Result<Item, ParseError> {
        let mut stmts: Vec<Item> = vec![];
        while self.peek()?.kind != TokenKind::RightBrace {
            stmts.push(self.statement()?);
        }
        Ok(Item::Block(stmts))
    }

    fn declaration(&mut self) -> Result<Item, ParseError> {
        self.consume(TokenKind::Let)?;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::Colon)?;
        let ty = self.consume(TokenKind::Type)?;
        self.consume(TokenKind::Equal)?;
        let value = self.expression()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Item::Decl(
            name.lexeme.unwrap(),
            Type::from(ty.lexeme.unwrap()),
            Box::new(value),
        ))
    }

    fn statement(&mut self) -> Result<Item, ParseError> {
        match self.peek()?.kind {
            TokenKind::Let => self.declaration(),
            TokenKind::Return => {
                self.consume(TokenKind::Return)?;
                let value = self.expression()?;
                self.consume(TokenKind::Semicolon)?;
                Ok(Item::Return(Box::new(value)))
            }
            _ => self.assignment(),
        }
    }

    fn assignment(&mut self) -> Result<Item, ParseError> {
        let mut item = self.expression()?;
        while self.peek()?.kind == TokenKind::Equal {
            self.consume(TokenKind::Equal)?;
            let right = self.expression()?;
            item = Item::Assign(Box::new(item), Box::new(right));
        }
        self.consume(TokenKind::Semicolon)?;
        Ok(item)
    }

    fn expression(&mut self) -> Result<Item, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Item, ParseError> {
        let mut expr = self.comparison()?;

        while matches!(
            self.peek()?.kind,
            TokenKind::BangEqual | TokenKind::EqualEqual
        ) {
            let operator = self.advance().unwrap();
            let right = self.comparison()?;
            expr = Item::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Item, ParseError> {
        let mut expr = self.term()?;

        while matches!(
            self.peek()?.kind,
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        ) {
            let operator = self.advance().unwrap();
            let right = self.term()?;
            expr = Item::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Item, ParseError> {
        let mut expr = self.factor()?;

        while matches!(self.peek()?.kind, TokenKind::Minus | TokenKind::Plus) {
            let operator = self.advance().unwrap();
            let right = self.factor()?;
            expr = Item::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Item, ParseError> {
        let mut expr = self.unary()?;

        while matches!(self.peek()?.kind, TokenKind::Slash | TokenKind::Star) {
            let operator = self.advance().unwrap();
            let right = self.unary()?;
            expr = Item::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Item, ParseError> {
        Ok(match self.peek()?.kind {
            TokenKind::Bang | TokenKind::Minus => {
                let operator = self.advance().unwrap();
                let right = self.unary()?;
                Item::Unary(operator, Box::new(right))
            }
            _ => self.access()?,
        })
    }

    fn access(&mut self) -> Result<Item, ParseError> {
        let mut item = self.call()?;

        while self.peek()?.kind == TokenKind::Dot {
            let token = self.consume(TokenKind::Dot)?;
            let right = self.call()?;
            item = Item::binary(item, token, right);
        }

        Ok(item)
    }

    fn call(&mut self) -> Result<Item, ParseError> {
        let mut item = self.primary()?;
        if self.peek()?.kind == TokenKind::LeftParen {
            self.consume(TokenKind::LeftParen)?;
            let mut args: Vec<Item> = vec![];
            if self.peek()?.kind != TokenKind::RightParen {
                args.push(self.expression()?);
                while self.peek()?.kind == TokenKind::Comma {
                    self.consume(TokenKind::Comma)?;
                    args.push(self.expression()?);
                }
            }
            self.consume(TokenKind::RightParen)?;
            item = Item::Call(Box::new(item), args);
        }
        Ok(item)
    }

    fn primary(&mut self) -> Result<Item, ParseError> {
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
                    "true" | "false" => Item::Bool(lexeme == "true"),
                    _ if lexeme.starts_with('"') => Item::Str(lexeme),
                    _ if lexeme.contains('.') => Item::Float(lexeme.parse().unwrap()),
                    _ if lexeme.chars().next().unwrap().is_ascii_digit() => {
                        Item::Int(lexeme.parse().unwrap())
                    }
                    e => unimplemented!("{:?}", e),
                }
            }
            TokenKind::Identifier => {
                let token = self.advance().unwrap();
                let lexeme = token.lexeme.unwrap();
                Item::Ident(lexeme)
            }
            _ => Err(ParseError::Unhandled(self.peek()?.kind, self.peek()?.span))?,
        })
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.eof() {
            return Err(ParseError::UnexpectedEof(
                self.tokens[self.current - 1].span,
            ));
        }

        let token = self.advance().unwrap();
        if token.kind == kind {
            Ok(token)
        } else {
            Err(ParseError::Expected(kind, token.span))
        }
    }

    fn eof(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Result<Token, ParseError> {
        let span = if self.current > 0 {
            self.tokens[self.current - 1].span
        } else {
            Span::new(0, 0)
        };
        match self.tokens.get(self.current).cloned() {
            Some(token) => Ok(token),
            None => Err(ParseError::UnexpectedEof(span)),
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.current += 1;
        self.tokens.get(self.current - 1).cloned()
    }
}

impl From<Vec<Token>> for Parser {
    fn from(tokens: Vec<Token>) -> Self {
        Self::new(tokens)
    }
}

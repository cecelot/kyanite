use inkwell::AddressSpace;

use crate::{
    ast::{
        node::{Call, FuncDecl, Ident},
        Node, Param,
    },
    token::{Span, Token, TokenKind},
    Ir,
};

use super::Builtin;

pub struct Println {}

impl Builtin for Println {
    fn build(&self, ir: &mut Ir<'_, '_>) {
        c_printf(ir);
        println("\"%s\n\"", ("str", "s"), ir);
        println("\"%d\n\"", ("int", "i"), ir);
        println("\"%f\n\"", ("float", "f"), ir);
    }
}

fn c_printf(ir: &mut Ir<'_, '_>) {
    let printf_type = ir.context.i32_type().fn_type(
        &[ir.context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .into()],
        true,
    );
    ir.module.add_function("printf", printf_type, None);
}

fn println(format: &str, ty: (&str, &str), ir: &mut Ir<'_, '_>) {
    let decl = FuncDecl {
        name: Token {
            kind: TokenKind::Identifier,
            lexeme: Some(format!("println_{}", ty.0)),
            span: Span::new(0, 0),
        },
        params: [Param {
            name: Token {
                kind: TokenKind::Identifier,
                lexeme: Some(ty.1.into()),
                span: Span::new(0, 0),
            },
            ty: Token {
                kind: TokenKind::Type,
                lexeme: Some(ty.0.into()),
                span: Span::new(0, 0),
            },
        }]
        .to_vec(),
        ty: Some(Token {
            kind: TokenKind::Type,
            lexeme: Some("void".into()),
            span: Span::new(0, 0),
        }),
        body: [Node::Call(Call {
            left: Box::new(Node::Ident(Ident {
                name: Token {
                    kind: TokenKind::Identifier,
                    lexeme: Some("printf".into()),
                    span: Span::new(0, 0),
                },
            })),
            args: [
                Node::Str(format.into()),
                Node::Ident(Ident {
                    name: Token {
                        kind: TokenKind::Identifier,
                        lexeme: Some(ty.1.into()),
                        span: Span::new(0, 0),
                    },
                }),
            ]
            .to_vec(),
            parens: (
                Token {
                    kind: TokenKind::LeftParen,
                    lexeme: None,
                    span: Span::new(0, 0),
                },
                Token {
                    kind: TokenKind::RightParen,
                    lexeme: None,
                    span: Span::new(0, 0),
                },
            ),
            delimiters: [Token {
                kind: TokenKind::Comma,
                lexeme: None,
                span: Span::new(0, 0),
            }]
            .to_vec(),
        })]
        .to_vec(),
    };
    ir.function(&decl).unwrap();
}

use super::{Expr, Stmt, Temp};

#[derive(Debug)]
pub struct Canon {
    ir: Vec<Stmt>,
}

/// Rewrite any [`Expr::Call`] to [`Expr::ESeq`] if it is not
/// the immediate child of a [`Stmt::Move`] or [`Stmt::Exp`]
trait CallRewrite<R> {
    fn rewrite(self, immediate: bool) -> R;
}

impl CallRewrite<Vec<Expr>> for Vec<Expr> {
    fn rewrite(self, immediate: bool) -> Vec<Expr> {
        self.into_iter()
            .map(|arg| {
                let temp = Temp::new();
                match arg {
                    Expr::Call(..) => Expr::ESeq {
                        stmt: Box::new(Stmt::Move {
                            target: Box::new(Expr::Temp(temp.clone())),
                            expr: Box::new(arg.rewrite(immediate)),
                        }),
                        expr: Box::new(Expr::Temp(temp)),
                    },
                    _ => arg,
                }
            })
            .collect()
    }
}

impl CallRewrite<Expr> for Expr {
    fn rewrite(self, immediate: bool) -> Expr {
        match self {
            Expr::Call(name, args) => {
                let args = args.rewrite(false);
                if !immediate {
                    let temp = Temp::new();
                    Expr::ESeq {
                        stmt: Box::new(Stmt::Seq {
                            left: Box::new(Stmt::Move {
                                target: Box::new(Expr::Temp(temp.clone())),
                                expr: Box::new(Expr::Call(name, args)),
                            }),
                            right: None,
                        }),
                        expr: Box::new(Expr::Temp(temp)),
                    }
                } else {
                    Expr::Call(name, args)
                }
            }
            Expr::Binary { left, right, op } => {
                let left = left.rewrite(false);
                let right = right.rewrite(false);
                Expr::Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    op,
                }
            }
            _ => self,
        }
    }
}

impl CallRewrite<Stmt> for Stmt {
    fn rewrite(self, _: bool) -> Stmt {
        match self {
            Stmt::Expr(e) => Stmt::Expr(Box::new(e.rewrite(true))),
            Stmt::Seq { left, right } => Stmt::Seq {
                left: Box::new(left.rewrite(false)),
                right: right.map(|item| Box::new(item.rewrite(false))),
            },
            Stmt::Move { target, expr } => Stmt::Move {
                target,
                expr: Box::new(expr.rewrite(true)),
            },
            Stmt::CJump {
                left,
                right,
                t,
                f,
                op,
            } => Stmt::CJump {
                left: Box::new(left.rewrite(false)),
                right: Box::new(right.rewrite(false)),
                t,
                f,
                op,
            },
            _ => self,
        }
    }
}

impl Canon {
    #[allow(dead_code)]
    pub fn new(ir: Vec<Stmt>) -> Self {
        Self { ir }
    }

    #[allow(dead_code)]
    pub fn canonicalize(mut self) -> Vec<Stmt> {
        // Replace all `Expr::Call` with `Expr::ESeq`
        self.ir = self
            .ir
            .into_iter()
            .map(|item| item.rewrite(false))
            .collect();
        // TODO: eliminate `Expr::ESeq`
        self.ir
    }
}

macro_rules! assert_ir {
    ($($path:expr => $name:ident),*) => {
        #[cfg(test)]
        mod tests {
            use std::collections::HashMap;

            use crate::{
                ast,
                kyir::{Translator, arch::amd64::Amd64},
                pass::{SymbolTable, TypeCheckPass},
                PipelineError, Source,
            };

            use super::Canon;

            $(
                #[test]
                fn $name() -> Result<(), Box<dyn std::error::Error>> {
                    let source = Source::new($path)?;
                    let ast = ast::Ast::from_source(&source)?;
                    let symbols = SymbolTable::from(&ast.nodes);
                    let mut accesses = HashMap::new();
                    let mut pass = TypeCheckPass::new(&symbols, &mut accesses, source, &ast.nodes);
                    pass.run().map_err(PipelineError::TypeError)?;
                    let mut translator: Translator<Amd64> = Translator::new(&accesses, &symbols);
                    let res = translator.translate(&ast.nodes);
                    let canon = Canon::new(res);
                    let res = canon.canonicalize();
                    insta::with_settings!({snapshot_path => "../../snapshots"}, {
                        insta::assert_debug_snapshot!(&res);
                    });

                    Ok(())
                }
            )*
        }
    };
}

// omit because serialized labels and temporaries may differ between runs
assert_ir!(
    // "test-cases/kyir/nested-calls.kya" => nested_calls
);

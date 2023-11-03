use std::collections::HashMap;

use crate::kyir::Expr;

use super::{blocks::BasicBlocks, eseq::ESeqs, rewrite::Rewrite, Stmt};

pub trait Extract {
    fn extract(self, ir: &mut Vec<Stmt>, replacements: &mut Vec<(usize, Box<Expr>)>);
}

impl Extract for Stmt {
    fn extract(self, ir: &mut Vec<Stmt>, replacements: &mut Vec<(usize, Box<Expr>)>) {
        match self {
            Stmt::Seq { left, right } => {
                left.extract(ir, replacements);
                if let Some(right) = right {
                    right.extract(ir, replacements);
                }
            }
            Stmt::Move { expr, target } => {
                update(&expr, ir, replacements);
                ir.push(Stmt::Move {
                    target: target.clone(),
                    expr: expr.clone(),
                });
            }
            Stmt::Label(_) => ir.push(self),
            Stmt::Noop => ir.push(self),
            Stmt::Jump(_) => ir.push(self),
            Stmt::Expr(_) => ir.push(self),
            Stmt::CJump {
                left,
                right,
                op,
                t,
                f,
            } => {
                update(&right, ir, replacements);
                update(&left, ir, replacements);
                ir.push(Stmt::CJump {
                    left: left.clone(),
                    right: right.clone(),
                    op,
                    t,
                    f,
                });
            }
        }
    }
}

fn update(expr: &Expr, ir: &mut Vec<Stmt>, replacements: &mut Vec<(usize, Box<Expr>)>) {
    let mut nested = vec![];
    expr.eseqs(&mut nested);
    for expr in nested.iter().rev() {
        if let Expr::ESeq {
            stmt,
            expr: temp,
            id: search,
        } = expr
        {
            if let Stmt::Seq { left, right } = *stmt.clone() {
                ir.push(*left);
                if let Some(right) = right {
                    ir.push(*right);
                }
            } else {
                ir.push(*stmt.clone());
            }
            replacements.push((*search, temp.clone()));
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug)]
pub struct Canon {
    ir: Vec<Stmt>,
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
        let mut ir = HashMap::new();
        let mut replacements = vec![];
        for item in self.ir.into_iter() {
            let name = item.label();
            let mut body = vec![];
            item.extract(&mut body, &mut replacements);
            ir.insert(name, body);
        }
        for (search, temp) in replacements {
            for item in ir.values_mut() {
                for stmt in item.iter_mut() {
                    stmt.replace(search, &temp);
                }
            }
        }
        let mut basic_blocks = BasicBlocks::new(ir);
        basic_blocks.build();
        dbg!(&basic_blocks.blocks);
        vec![]
    }
}

// FIXME: omit because serialized labels and temporaries may differ between runs
// macro_rules! assert_ir {
//     ($($path:expr => $name:ident),*) => {
//         #[cfg(test)]
//         mod tests {
//             use std::collections::HashMap;

//             use crate::{
//                 ast,
//                 kyir::{Translator, arch::amd64::Amd64},
//                 pass::{SymbolTable, TypeCheckPass},
//                 PipelineError, Source,
//             };

//             use super::Canon;

//             $(
//                 #[test]
//                 fn $name() -> Result<(), Box<dyn std::error::Error>> {
//                     let source = Source::new($path)?;
//                     let ast = ast::Ast::from_source(&source)?;
//                     let symbols = SymbolTable::from(&ast.nodes);
//                     let mut accesses = HashMap::new();
//                     let mut pass = TypeCheckPass::new(&symbols, &mut accesses, source, &ast.nodes);
//                     pass.run().map_err(PipelineError::TypeError)?;
//                     let mut translator: Translator<Amd64> = Translator::new(&accesses, &symbols);
//                     let res = translator.translate(&ast.nodes);
//                     let canon = Canon::new(res);
//                     let canonicalized = canon.canonicalize();
//                     dbg!(&canonicalized);
//                     // insta::with_settings!({snapshot_path => "../../snapshots"}, {
//                     //     insta::assert_debug_snapshot!(&res);
//                     // });

//                     Ok(())
//                 }
//             )*
//         }
//     };
// }

// assert_ir!(
//     "test-cases/kyir/nested-calls.kya" => nested_calls
// );

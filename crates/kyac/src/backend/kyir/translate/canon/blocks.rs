use crate::backend::kyir::{
    ir::Stmt,
    translate::{canon::rewrite::Substitute, Jump},
};
use std::collections::VecDeque;

#[derive(Debug)]
pub struct BasicBlocks {
    inner: Vec<BasicBlock>,
    functions: Vec<(String, Vec<Stmt>)>,
}

impl BasicBlocks {
    pub fn new(functions: Vec<(String, Vec<Stmt>)>) -> Self {
        let mut substitutions = vec![];
        let mut blocks = Self {
            functions,
            inner: vec![],
        };
        blocks.build(&mut substitutions);
        blocks.substitute(&substitutions);
        blocks
    }

    fn build(&mut self, substitutions: &mut Vec<(String, String)>) {
        for (_, function) in &mut self.functions {
            let name = Self::consume(function);
            assert!(matches!(name, Stmt::Label(_)));
            let mut blocks = Self::block(name.label(), function, substitutions, &name);
            self.inner.append(&mut blocks);
        }
    }

    fn block(
        label: String,
        function: &mut Vec<Stmt>,
        substitutions: &mut Vec<(String, String)>,
        func: &Stmt,
    ) -> Vec<BasicBlock> {
        let mut blocks = vec![];
        let mut body = vec![];
        let mut jump = true;
        while let Some(stmt) = Self::peek(function) {
            match stmt {
                Stmt::Label(l) => {
                    Self::consume(function);
                    if let Some(Stmt::Label(ll)) = Self::peek(function) {
                        substitutions.push((ll.name, l.clone().name));
                    }
                    if !body.is_empty() {
                        body.push(Jump::wrapped(l.clone().name));
                        // Process the next block
                        let mut rest = Self::block(l.name, function, substitutions, func);
                        blocks.append(&mut rest);
                        // Stop processing this block
                        jump = false;
                        break;
                    }
                }
                Stmt::CJump(cjmp) => {
                    body.push(Self::consume(function));
                    // Create a new block for the true branch and process it
                    let mut t = Self::block(cjmp.t.clone(), function, substitutions, func);
                    // Create a new block for the false branch and process it
                    let mut f = Self::block(cjmp.f.clone(), function, substitutions, func);
                    blocks.append(&mut t);
                    blocks.append(&mut f);
                    jump = false;
                    // Stop processing this block
                    break;
                }
                Stmt::Jump(_) => {
                    body.push(Self::consume(function));
                    jump = false;
                    // Stop processing this block
                    break;
                }
                _ => body.push(Self::consume(function)),
            };
        }
        if jump {
            // If the epilogue of the function occurs many jumps later, ensure that
            // that block jumps to the *function* epilogue, not a non-existent *block*
            // epilogue (i.e. L2.epilogue instead of main.epilogue)
            body.push(Jump::wrapped(format!("{}.epilogue", func.label()))); // Jump to function epilogue
        }
        blocks.push(BasicBlock::new(body, label));
        blocks
    }

    fn peek(function: &[Stmt]) -> Option<Stmt> {
        function.first().cloned()
    }

    fn consume(function: &mut Vec<Stmt>) -> Stmt {
        function.remove(0)
    }

    pub fn inner(&self) -> Vec<BasicBlock> {
        self.inner.clone()
    }
}

impl Substitute for BasicBlocks {
    fn substitute(&mut self, substitutions: &[(String, String)]) {
        for block in &mut self.inner {
            for stmt in &mut block.body {
                stmt.substitute(substitutions);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub body: Vec<Stmt>,
    pub label: String,
}

impl BasicBlock {
    pub fn new(body: Vec<Stmt>, label: String) -> Self {
        Self { body, label }
    }

    pub fn successors<'a>(&self, blocks: &'a VecDeque<BasicBlock>) -> Vec<&'a BasicBlock> {
        blocks
            .iter()
            .filter(|block| match self.body.last() {
                Some(Stmt::Jump(l)) => l.target == block.label,
                Some(Stmt::CJump(cjmp)) => cjmp.t == block.label || cjmp.f == block.label,
                _ => false,
            })
            .collect()
    }
}

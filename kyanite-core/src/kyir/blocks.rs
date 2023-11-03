use std::collections::HashMap;

use super::Stmt;

#[derive(Debug)]
pub struct BasicBlock {
    pub body: Vec<Stmt>,
    pub label: Stmt,
}

impl BasicBlock {
    pub fn new(label: Stmt, body: Vec<Stmt>) -> Self {
        Self { label, body }
    }
}

#[derive(Debug)]
pub struct BasicBlocks {
    pub(super) blocks: Vec<BasicBlock>,
    functions: HashMap<String, Vec<Stmt>>,
}

impl BasicBlocks {
    pub fn new(functions: HashMap<String, Vec<Stmt>>) -> Self {
        Self {
            functions,
            blocks: vec![],
        }
    }

    pub fn build(&mut self) {
        for function in self.functions.values_mut() {
            let name = Self::consume(function);
            assert!(matches!(name, Stmt::Label(_)));
            let mut blocks = Self::block(name.clone(), function, &name);
            self.blocks.append(&mut blocks);
        }
    }

    fn block(label: Stmt, function: &mut Vec<Stmt>, func: &Stmt) -> Vec<BasicBlock> {
        let mut blocks = vec![];
        let mut body = vec![];
        let mut jump = true;
        while let Some(stmt) = Self::peek(function) {
            match stmt {
                Stmt::Label(l) => {
                    Self::consume(function);
                    if !body.is_empty() {
                        body.push(Stmt::Jump(l.clone()));
                        // Process the next block
                        let mut rest = Self::block(Stmt::Label(l), function, func);
                        blocks.append(&mut rest);
                        // Stop processing this block
                        jump = false;
                        break;
                    }
                }
                Stmt::CJump { t, f, .. } => {
                    body.push(Self::consume(function));
                    // Create a new block for the true branch and process it
                    let mut t = Self::block(Stmt::Label(t.clone()), function, func);
                    // Create a new block for the false branch and process it
                    let mut f = Self::block(Stmt::Label(f.clone()), function, func);
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
            body.push(Stmt::Jump(format!("{}.epilogue", func.label()))); // Jump to function epilogue
        }
        blocks.push(BasicBlock::new(label, body));
        blocks
    }

    fn peek(function: &[Stmt]) -> Option<Stmt> {
        function.get(0).cloned()
    }

    fn consume(function: &mut Vec<Stmt>) -> Stmt {
        function.remove(0)
    }
}

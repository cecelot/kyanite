use std::collections::{HashMap, VecDeque};

use super::Stmt;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub body: Vec<Stmt>,
    pub label: String,
}

impl BasicBlock {
    pub fn new(label: String, body: Vec<Stmt>) -> Self {
        Self { label, body }
    }

    pub fn successors<'a>(&self, blocks: &'a VecDeque<BasicBlock>) -> Vec<&'a BasicBlock> {
        blocks
            .iter()
            .filter(|block| match self.body.last() {
                Some(Stmt::Jump(l)) => l == &block.label,
                Some(Stmt::CJump { t, f, .. }) => t == &block.label || f == &block.label,
                _ => unreachable!(),
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct BasicBlocks {
    pub(super) blocks: Vec<BasicBlock>,
    pub(super) substitutions: Vec<(String, String)>,
    functions: HashMap<String, Vec<Stmt>>,
}

impl BasicBlocks {
    pub fn new(functions: HashMap<String, Vec<Stmt>>) -> Self {
        let mut substitutions = vec![];
        let mut blocks = Self {
            functions,
            blocks: vec![],
            substitutions: vec![],
        };
        blocks.build(&mut substitutions);
        blocks.substitutions = substitutions;
        blocks
    }

    fn build(&mut self, substitutions: &mut Vec<(String, String)>) {
        for function in self.functions.values_mut() {
            let name = Self::consume(function);
            assert!(matches!(name, Stmt::Label(_)));
            let mut blocks = Self::block(name.label(), function, substitutions, &name);
            self.blocks.append(&mut blocks);
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
                        substitutions.push((ll, l.clone()));
                    }
                    if !body.is_empty() {
                        body.push(Stmt::Jump(l.clone()));
                        // Process the next block
                        let mut rest = Self::block(l, function, substitutions, func);
                        blocks.append(&mut rest);
                        // Stop processing this block
                        jump = false;
                        break;
                    }
                }
                Stmt::CJump { t, f, .. } => {
                    body.push(Self::consume(function));
                    // Create a new block for the true branch and process it
                    let mut t = Self::block(t.clone(), function, substitutions, func);
                    // Create a new block for the false branch and process it
                    let mut f = Self::block(f.clone(), function, substitutions, func);
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

#[derive(Debug)]
pub struct TraceSchedule {
    pub(super) traces: Vec<Vec<BasicBlock>>,
}

impl TraceSchedule {
    pub fn new(mut blocks: VecDeque<BasicBlock>) -> Self {
        let mut marks: HashMap<String, bool> = HashMap::new();
        let mut traces = vec![];
        while !blocks.is_empty() {
            let mut trace = vec![];
            let mut block = blocks.pop_front().unwrap();
            while !marks.get(&block.label.clone()).copied().unwrap_or(false) {
                marks.insert(block.label.clone(), true);
                let successors = block.successors(&blocks);
                let mut successors: Vec<_> = successors
                    .iter()
                    .filter(|block| !marks.get(&block.label).copied().unwrap_or(false))
                    .collect();
                trace.push(block.clone());
                if !successors.is_empty() {
                    let end = block.body.last().unwrap();
                    let first = if let Stmt::CJump { f, .. } = end {
                        let next = successors.iter().find(|block| block.label == **f).unwrap();
                        (**next).clone()
                    } else {
                        (**successors.remove(0)).clone()
                    };
                    block = first;
                } else if !blocks.is_empty() {
                    block = blocks.pop_front().unwrap();
                } else {
                    break;
                }
            }
            traces.push(trace);
        }
        Self { traces }
    }
}

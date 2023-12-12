use std::collections::{HashMap, VecDeque};

use crate::kyir::Label;

use super::{rewrite::Substitute, Stmt};

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
                _ => false,
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct BasicBlocks {
    pub(super) blocks: Vec<BasicBlock>,
    functions: Vec<(String, Vec<Stmt>)>,
}

impl Substitute for BasicBlocks {
    fn substitute(&mut self, substitutions: &[(String, String)]) {
        for block in &mut self.blocks {
            for stmt in &mut block.body {
                stmt.substitute(substitutions);
            }
        }
    }
}

impl BasicBlocks {
    pub fn from_functions(functions: Vec<(String, Vec<Stmt>)>) -> Vec<BasicBlock> {
        let mut substitutions = vec![];
        let mut builder = Self {
            functions,
            blocks: vec![],
        };
        builder.build(&mut substitutions);
        builder.substitute(&substitutions);
        builder.blocks
    }

    fn build(&mut self, substitutions: &mut Vec<(String, String)>) {
        for (_, function) in self.functions.iter_mut() {
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
pub struct TraceSchedule(Vec<Vec<BasicBlock>>);

impl TraceSchedule {
    pub fn from_blocks(blocks: VecDeque<BasicBlock>) -> Vec<Stmt> {
        let mut traces = Self::traces(blocks);
        Self::unconditionals(&mut traces);
        Self::conditionals(&mut traces);
        let mut blocks = Self::blocks(traces);
        let main = blocks.pop().unwrap();
        let jmp = main.body.last().unwrap();
        if let Stmt::CJump { ref f, .. } = jmp {
            let idx = blocks
                .iter()
                .map(|block| block.label.clone())
                .position(|label| label == *f)
                .unwrap();
            blocks.insert(idx, main);
            let (left, right) = blocks.split_at(idx);
            Self::stmts(right.iter().cloned().chain(left.to_vec()).collect())
        } else {
            blocks.insert(0, main);
            Self::stmts(blocks)
        }
    }

    fn conditionals(traces: &mut [Vec<BasicBlock>]) {
        let insertions = Self::insertions(traces);
        for insertion in insertions {
            let ((i, j), block) = insertion;
            traces[i].insert(j + 1, block);
        }
    }

    fn insertions(traces: &mut [Vec<BasicBlock>]) -> Vec<((usize, usize), BasicBlock)> {
        let labels: Vec<Vec<_>> = traces
            .iter()
            .map(|trace| trace.iter().map(|block| block.label.clone()).collect())
            .collect();
        let mut insertions = vec![];
        for (i, trace) in traces.iter_mut().enumerate() {
            for (j, block) in trace.iter_mut().enumerate() {
                let next = labels.get(i).and_then(|labels| labels.get(j + 1));
                if let Some(Stmt::CJump { f, .. }) = block.body.last_mut() {
                    if let Some(next) = next {
                        if f != next {
                            let prev = f.clone();
                            let label = Label::new();
                            *f = label.clone();
                            insertions
                                .push(((i, j), BasicBlock::new(label, vec![Stmt::Jump(prev)])));
                        }
                    }
                }
            }
        }
        insertions
    }

    fn unconditionals(traces: &mut [Vec<BasicBlock>]) {
        for trace in traces {
            if trace.len() >= 2 {
                for i in 0..trace.len() - 2 {
                    let j = i + 1;
                    let right = trace[j].label.clone();
                    let left = &mut trace[i];
                    if let (Some(Stmt::Jump(l)), r) = (&left.body.last(), &right) {
                        if l == r {
                            left.body.remove(left.body.len() - 1);
                        }
                    }
                }
            }
        }
    }

    fn traces(mut blocks: VecDeque<BasicBlock>) -> Vec<Vec<BasicBlock>> {
        let mut traces = vec![];
        let mut marks: HashMap<String, bool> = HashMap::new();
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
        traces
    }

    fn blocks(traces: Vec<Vec<BasicBlock>>) -> Vec<BasicBlock> {
        traces
            .into_iter()
            .flat_map(|trace| trace.into_iter())
            .collect()
    }

    fn stmts(blocks: Vec<BasicBlock>) -> Vec<Stmt> {
        blocks
            .into_iter()
            .flat_map(|block| vec![Stmt::Label(block.label)].into_iter().chain(block.body))
            .collect()
    }
}

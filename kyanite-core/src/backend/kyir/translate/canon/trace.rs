use crate::backend::kyir::{
    translate::canon::blocks::BasicBlock,
    translate::{Label, Stmt},
};
use std::collections::{HashMap, VecDeque};

pub fn new(blocks: VecDeque<BasicBlock>) -> Vec<Stmt> {
    let mut traces = traces(blocks);
    self::unconditionals(&mut traces);
    self::conditionals(&mut traces);
    let mut blocks = self::blocks(traces);
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
        self::stmts(right.iter().cloned().chain(left.to_vec()).collect())
    } else {
        blocks.insert(0, main);
        self::stmts(blocks)
    }
}

/// Inserts blocks which jump to the false branch of conditional jumps where the false branch is not the next block
/// for consistency.
fn conditionals(traces: &mut [Vec<BasicBlock>]) {
    /// Returns a list of blocks to insert into the relevant trace.
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
                                .push(((i, j), BasicBlock::new(vec![Stmt::Jump(prev)], label)));
                        }
                    }
                }
            }
        }
        insertions
    }
    let insertions = insertions(traces);
    for insertion in insertions {
        let ((i, j), block) = insertion;
        traces[i].insert(j + 1, block);
    }
}

/// Removes jumps from the end of blocks that jump to the next block.
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

/// Returns an ordered list of traces from a list of blocks.
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
        .flat_map(IntoIterator::into_iter)
        .collect()
}

fn stmts(blocks: Vec<BasicBlock>) -> Vec<Stmt> {
    blocks
        .into_iter()
        .flat_map(|block| vec![Stmt::Label(block.label)].into_iter().chain(block.body))
        .collect()
}

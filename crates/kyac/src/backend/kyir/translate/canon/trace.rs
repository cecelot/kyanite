use crate::backend::kyir::{
    translate::canon::blocks::BasicBlock,
    translate::{Jump, Label, Stmt},
};
use std::collections::{HashMap, VecDeque};

pub fn new(blocks: VecDeque<BasicBlock>) -> Vec<Stmt> {
    let mut traces = traces(blocks);
    self::unconditionals(&mut traces);
    self::conditionals(&mut traces);
    let blocks = self::blocks(traces);
    self::stmts(blocks)
}

/// Inserts blocks which jump to the false branch of conditional jumps where the false branch is not the next block
/// for consistency.
fn conditionals(traces: &mut [Vec<BasicBlock>]) {
    let labels: Vec<Vec<_>> = traces
        .iter()
        .map(|trace| trace.iter().map(|block| block.label.clone()).collect())
        .collect();
    for (i, trace) in traces.iter_mut().enumerate() {
        for (j, block) in trace.iter_mut().enumerate() {
            let next = labels.get(i).and_then(|labels| labels.get(j + 1));
            if let Some(Stmt::CJump(jmp)) = block.body.last_mut() {
                // If the false branch is not the next block, insert a block that jumps to it. Alternatively, if there is
                // no next block in this trace, insert a block that jumps to the false branch.
                if next.is_some_and(|next| *next != jmp.f) || next.is_none() {
                    let prev = jmp.f.clone();
                    let label = Label::next();
                    jmp.f = label.clone();
                    block
                        .body
                        .append(&mut vec![Label::wrapped(label), Jump::wrapped(prev)]);
                }
            }
        }
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
                    if l.target == *r {
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
                let first = if let Stmt::CJump(jmp) = end {
                    let next = successors
                        .iter()
                        .find(|block| block.label == jmp.f)
                        .unwrap();
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
        .flat_map(|block| {
            vec![Label::wrapped(block.label)]
                .into_iter()
                .chain(block.body)
        })
        .collect()
}

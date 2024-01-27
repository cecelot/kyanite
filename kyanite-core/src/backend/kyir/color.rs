use std::collections::{HashMap, HashSet};

use crate::backend::kyir::arch::Frame;

use super::liveness::{InterferenceGraph, LiveRanges};

pub struct Color<F: Frame> {
    interferences: Vec<InterferenceGraph>,
    _frame: std::marker::PhantomData<F>,
}

impl<F: Frame> Color<F> {
    pub fn new(interferences: Vec<InterferenceGraph>) -> Self {
        Self {
            interferences,
            _frame: std::marker::PhantomData,
        }
    }

    pub fn color(&self, ranges: &LiveRanges) -> HashMap<String, String> {
        let mut colors = HashMap::new();
        let temporaries: Vec<_> = ranges.keys().collect();
        let registers = F::registers();
        let registers: Vec<String> = registers
            .temporary
            .iter()
            .chain(registers.callee.iter())
            .map(|&reg| String::from(reg))
            .collect();
        for (line, graph) in self.interferences.iter().enumerate() {
            let mut live: Vec<&String> = temporaries
                .iter()
                .filter(|&t| ranges.get(t)[line])
                .copied()
                .collect();
            live.sort_by_key(|&t| graph.get(t).map_or(0, HashSet::len));
            while let Some(temp) = live.pop() {
                if !colors.contains_key(temp) {
                    let empty = HashSet::new();
                    let used: Vec<_> = graph
                        .get(temp)
                        .unwrap_or(&empty)
                        .iter()
                        .map(|t| colors.get(t))
                        .collect();
                    let color = registers
                        .iter()
                        .find(|&r| !used.contains(&Some(r)))
                        .expect("ran out of registers");
                    colors.insert(temp.clone(), color.clone());
                }
            }
        }
        colors
    }
}

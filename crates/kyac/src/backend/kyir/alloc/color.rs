use crate::backend::kyir::{
    alloc::liveness::{InterferenceGraph, LiveRanges},
    arch::{ArchInstr, Frame},
};
use std::collections::{HashMap, HashSet};

pub struct Color<I: ArchInstr, F: Frame<I>> {
    interferences: Vec<InterferenceGraph>,
    _phantom: std::marker::PhantomData<(F, I)>,
}

impl<I: ArchInstr, F: Frame<I>> Color<I, F> {
    pub fn new(interferences: Vec<InterferenceGraph>) -> Self {
        Self {
            interferences,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn color(&self, ranges: &LiveRanges) -> HashMap<String, String> {
        let mut colors = HashMap::new();
        let temporaries: Vec<_> = ranges.keys().collect();
        let registers: Vec<String> = F::registers()
            .temporary
            .iter()
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
        log::trace!("register mapping: {colors:#?}");
        colors
    }
}

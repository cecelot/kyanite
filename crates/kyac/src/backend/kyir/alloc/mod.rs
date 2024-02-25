mod color;
mod liveness;

use crate::backend::kyir::{
    alloc::{
        color::Color,
        liveness::{Graph, LiveRanges},
    },
    arch::{ArchInstr, Frame},
    AsmInstr,
};
use std::collections::HashMap;

pub fn registers<I: ArchInstr, F: Frame<I>>(instrs: &Vec<AsmInstr<I>>) -> Registers {
    let graph = Graph::from(instrs);
    let ranges = LiveRanges::from(graph);
    let ig = ranges.interference_graphs(instrs.len());
    let color: Color<I, F> = Color::new(ig);
    Registers(color.color(&ranges))
}

crate::newtype!(Registers:HashMap<String, String>);

impl Registers {
    pub fn get(&self, temp: String) -> String {
        self.0.get(&temp).cloned().unwrap_or(temp)
    }
}

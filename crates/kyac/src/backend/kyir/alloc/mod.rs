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
    let interferences = ranges.interferences();
    let color: Color<I, F> = Color::new(interferences);
    Registers(color.color(&ranges, instrs.len()))
}

crate::newtype!(Registers:HashMap<String, String>);

impl Registers {
    pub fn get<I: ArchInstr, F: Frame<I>>(&self, temp: String) -> String {
        self.0
            .get(&temp)
            .cloned()
            .unwrap_or(if temp.starts_with('T') {
                // This temporary register is never used, but we need to
                // allocate it to some register
                F::registers().discard.into()
            } else {
                temp
            })
    }
}

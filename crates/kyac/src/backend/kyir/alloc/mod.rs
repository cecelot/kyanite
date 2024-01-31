mod color;
mod liveness;

use crate::backend::kyir::{
    alloc::{
        color::Color,
        liveness::{Graph, LiveRanges},
    },
    arch::Frame,
    AsmInstr,
};
use std::collections::HashMap;

pub fn registers<F: Frame>(asm: &Vec<AsmInstr>) -> HashMap<String, String> {
    let graph = Graph::from(asm);
    let ranges = LiveRanges::from(graph);
    let ig = ranges.interference_graphs(asm.len());
    let color: Color<F> = Color::new(ig);
    color.color(&ranges)
}

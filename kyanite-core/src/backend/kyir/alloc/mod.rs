mod color;
mod liveness;

use crate::backend::kyir::{
    alloc::{
        color::Color,
        liveness::{Graph, LiveRanges},
    },
    arch::{amd64::Amd64, Frame},
    AsmInstr,
};
use std::collections::HashMap;

pub fn registers<F: Frame>(asm: &Vec<AsmInstr>) -> HashMap<String, String> {
    let graph = Graph::from(asm);
    let ranges = LiveRanges::from(graph);
    let ig = ranges.interference_graphs(asm.len());
    let color: Color<Amd64> = Color::new(ig);
    color.color(&ranges)
}

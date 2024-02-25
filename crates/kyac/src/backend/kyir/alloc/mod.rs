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
    pub fn get<I: ArchInstr, F: Frame<I>>(&self, temp: &String) -> String {
        match temp {
            _ if temp.starts_with('T') => {
                let register = self.0.get(temp).cloned().unwrap_or_else(|| {
                    panic!("no register for `{temp}`");
                });
                register
            }
            _ if temp.starts_with("[T") => {
                let temp = temp.replace(['[', ']'], "");
                let register = self.0.get(&temp).cloned().unwrap_or_else(|| {
                    panic!("no register for `{temp}`");
                });
                format!("[{register}]")
            }
            _ if F::registers().all().contains(&temp.as_str()) => temp.to_string(),
            _ if temp.is_empty() => String::new(),
            temp => temp.to_string(),
        }
    }
}

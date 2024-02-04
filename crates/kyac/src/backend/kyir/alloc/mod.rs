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

pub fn registers<F: Frame>(instrs: &Vec<AsmInstr>) -> Registers {
    let graph = Graph::from(instrs);
    let ranges = LiveRanges::from(graph);
    let ig = ranges.interference_graphs(instrs.len());
    let color: Color<F> = Color::new(ig);
    Registers(color.color(&ranges))
}

pub struct Registers(HashMap<String, String>);

impl Registers {
    pub fn get<F: Frame>(&self, temp: &String) -> String {
        match temp {
            _ if temp.starts_with('T') => {
                let register = self.0.get(temp).cloned().unwrap_or_else(|| {
                    panic!("no register for `{temp}`");
                });
                format!("%{register}")
            }
            _ if temp.starts_with('(') => {
                let temp = temp.replace(['(', ')'], "");
                let register = self.0.get(&temp).cloned().unwrap_or_else(|| {
                    panic!("no register for `{temp}`");
                });
                format!("(%{register})")
            }
            _ if F::registers().all().contains(&temp.as_str()) => format!("%{temp}"),
            _ if temp.is_empty() => String::new(),
            temp => temp.to_string(),
        }
    }
}

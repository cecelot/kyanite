use crate::backend::kyir::{
    arch::{ArchInstr, FlowGraphMeta},
    AsmInstr,
};
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Default)]
pub struct Graph<'a, I: ArchInstr> {
    adj: HashMap<usize, Vec<usize>>,
    instrs: &'a [AsmInstr<I>],
}

impl<'a, I: ArchInstr> Graph<'a, I> {
    pub fn new(instrs: &'a [AsmInstr<I>]) -> Self {
        Self {
            instrs,
            adj: HashMap::new(),
        }
    }

    pub fn add(&mut self, stmt: usize, next: usize) {
        self.adj.entry(stmt).or_default().push(next);
    }

    pub fn temporaries(&self) -> HashSet<String> {
        self.instrs
            .iter()
            .flat_map(|v| v.uses().into_iter().chain(v.defines()))
            .filter(|x| x.starts_with('T'))
            .collect()
    }

    fn uses(&self, temp: &String) -> Vec<usize> {
        self.instrs
            .iter()
            .enumerate()
            .filter(|(_, v)| v.uses().contains(temp))
            .map(|(k, _)| k)
            .collect()
    }

    fn defines(&self, cur: usize, temp: &String) -> bool {
        let cur = self.instrs.get(cur).unwrap();
        cur.defines().contains(temp)
    }

    fn predecessors(&self, cur: usize) -> impl IntoIterator<Item = usize> + '_ {
        self.adj
            .iter()
            .filter(move |(_, v)| v.contains(&cur))
            .map(|(k, _)| *k)
    }

    pub fn liveness(&self, temp: &String) -> Vec<bool> {
        let mut live = vec![false; self.adj.len()];
        for site in self.uses(temp) {
            live[site] = true;
            let mut worklist = VecDeque::new();
            worklist.push_back(site);
            while !worklist.is_empty() {
                let cur = worklist.pop_front().unwrap();
                if !self.defines(cur, temp) {
                    live[cur] = true;
                    for predecessor in self.predecessors(cur) {
                        worklist.push_back(predecessor);
                    }
                }
            }
        }
        live
    }
}

crate::newtype!(LiveRanges:HashMap<String, Vec<bool>>);

impl LiveRanges {
    pub fn get(&self, temp: &str) -> &Vec<bool> {
        self.0.get(temp).unwrap()
    }

    pub fn interferences(&self) -> HashMap<String, HashSet<String>> {
        let mut interferes = HashMap::new();
        for (temp, range) in &self.0 {
            interferes.insert(temp.clone(), HashSet::new());
            let temp_lines = Self::lines(range);
            for (other, range) in self.0.iter().filter(|&(k, _)| k != temp) {
                let other_lines = Self::lines(range);
                let overlaps = temp_lines
                    .iter()
                    .filter(|x| other_lines.contains(x))
                    .count();
                if overlaps > 0 {
                    interferes.entry(temp.clone()).and_modify(|l| {
                        l.insert(other.clone());
                    });
                }
            }
        }
        interferes
    }

    fn lines(range: &[bool]) -> Vec<usize> {
        range
            .iter()
            .enumerate()
            .filter(|&(_, v)| *v)
            .map(|(i, _)| i)
            .collect()
    }
}

impl<I: ArchInstr> From<Graph<'_, I>> for LiveRanges {
    fn from(graph: Graph<'_, I>) -> Self {
        let ranges = graph
            .temporaries()
            .iter()
            .map(|temp| (temp.to_string(), graph.liveness(temp)))
            .collect();
        Self(ranges)
    }
}

impl<'a, I: ArchInstr> From<&'a Vec<AsmInstr<I>>> for Graph<'a, I> {
    fn from(instrs: &'a Vec<AsmInstr<I>>) -> Self {
        let mut graph = Self::new(instrs);
        let mut worklist = VecDeque::from(vec![0]);
        let mut visited: HashSet<usize> = HashSet::new();
        while !worklist.is_empty() {
            let stmt = worklist.pop_front().unwrap();
            let instr = &instrs[stmt];
            let successors = if let Some(label) = instr.to() {
                let idx = instrs
                    .iter()
                    .position(|x| x.label().map_or(false, |name| label == *name))
                    .unwrap();
                vec![idx, stmt + 1]
            } else {
                vec![stmt + 1]
            };
            for next in successors {
                graph.add(stmt, next);
                if !visited.contains(&next) && next < instrs.len() - 1 {
                    worklist.push_back(next);
                    visited.insert(next);
                }
            }
        }
        restore(instrs, &mut graph);
        graph
    }
}

fn restore<I: ArchInstr>(instrs: &[AsmInstr<I>], graph: &mut Graph<I>) {
    for (&from, to) in &mut graph.adj {
        if instrs[from].jump() {
            let next = &instrs[from + 1];
            to.retain(|&x| x != from + 1);
            next.label()
                .is_some_and(|x| x == instrs[from].to().unwrap())
                .then(|| {
                    to.push(from + 1);
                });
        }
    }
}

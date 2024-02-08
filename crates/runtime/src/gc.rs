use std::collections::HashMap;

#[derive(Default, Debug)]
pub struct GarbageCollector {
    map: HashMap<String, String>,
    state: i32,
}

impl GarbageCollector {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            state: 0,
        }
    }

    pub fn run(&mut self) {
        self.map
            .insert(format!("{}hello", self.state), "world".to_string());
        self.state += 1;
        // dbg!(&self.map);
    }
}

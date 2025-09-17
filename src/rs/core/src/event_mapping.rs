use std::collections::HashMap;

pub struct EventMapping {
    pub events: HashMap<String, String>,
}

impl EventMapping {
    pub fn new() -> Self {
        Self {
            events: HashMap::new(),
        }
    }
}
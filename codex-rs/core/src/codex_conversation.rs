use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimpleCodexConversation {
    pub id: String,
    pub messages: Vec<String>,
}

impl SimpleCodexConversation {
    pub fn new(id: String) -> Self {
        Self {
            id,
            messages: Vec::new(),
        }
    }
}
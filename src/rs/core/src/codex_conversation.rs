use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodexConversation {
    pub id: String,
    pub messages: Vec<String>,
}

impl CodexConversation {
    pub fn new(id: String) -> Self {
        Self {
            id,
            messages: Vec::new(),
        }
    }
}
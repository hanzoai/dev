use serde::{Deserialize, Serialize};
use crate::protocol::InputMessageKind;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ResponseInputItem {
    pub content: Vec<ContentItem>,
    pub kind: InputMessageKind,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum ContentItem {
    Text(String),
    Image { data: Vec<u8>, mime_type: String },
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ResponseItem {
    pub content: Vec<ContentItem>,
}

impl From<ResponseInputItem> for ResponseItem {
    fn from(input: ResponseInputItem) -> Self {
        ResponseItem {
            content: input.content,
        }
    }
}
//! Wire protocol definitions

use serde::{Deserialize, Serialize};

/// Supported wire protocols
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum WireProtocol {
    /// OpenAI Chat Completions API
    Chat,
    /// OpenAI Responses API (with reasoning)
    Responses,
    /// Legacy completion API
    Completion,
}

/// Generic request wrapper
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request<T> {
    pub id: uuid::Uuid,
    pub protocol: WireProtocol,
    pub payload: T,
}

/// Generic response wrapper
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response<T> {
    pub id: uuid::Uuid,
    pub request_id: uuid::Uuid,
    pub payload: T,
}

impl<T> Request<T> {
    pub fn new(protocol: WireProtocol, payload: T) -> Self {
        Self {
            id: uuid::Uuid::new_v4(),
            protocol,
            payload,
        }
    }
}

impl<T> Response<T> {
    pub fn new(request_id: uuid::Uuid, payload: T) -> Self {
        Self {
            id: uuid::Uuid::new_v4(),
            request_id,
            payload,
        }
    }
}
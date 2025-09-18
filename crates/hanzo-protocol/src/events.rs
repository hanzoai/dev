//! Event stream definitions

use serde::{Deserialize, Serialize};
use std::pin::Pin;
use std::task::{Context, Poll};

/// Events that can occur during streaming
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Event {
    /// Text delta from the model
    TextDelta { text: String },

    /// Tool invocation request
    ToolCall {
        id: String,
        name: String,
        arguments: serde_json::Value,
    },

    /// Tool execution result
    ToolResult {
        call_id: String,
        output: serde_json::Value,
    },

    /// Reasoning token (for models that support it)
    Reasoning { text: String },

    /// Stream completed
    Complete,

    /// Error occurred
    Error { message: String },
}

/// Trait for event streams
pub trait EventStream: Send + Sync {
    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Event>>;
}

/// Boxed event stream for type erasure
pub type BoxedEventStream = Pin<Box<dyn EventStream>>;

impl<S> EventStream for S
where
    S: futures::Stream<Item = Event> + Send + Sync,
{
    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Event>> {
        futures::Stream::poll_next(self, cx)
    }
}
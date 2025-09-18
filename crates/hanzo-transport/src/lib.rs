//! Transport layer for network communication

use async_trait::async_trait;
use std::error::Error;

pub mod http;
pub mod websocket;

#[async_trait]
pub trait Transport: Send + Sync {
    type Error: Error + Send + Sync + 'static;

    async fn send(&self, request: Vec<u8>) -> Result<Vec<u8>, Self::Error>;
}
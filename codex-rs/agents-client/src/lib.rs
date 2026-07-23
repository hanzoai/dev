//! Client for the Hanzo Cloud `/v1/agents` registry — per-org autonomous agent
//! definitions and their runs. Mirrors the shape of `codex-cloud-tasks-client`:
//! a backend trait, wire types, and a `reqwest`-based HTTP implementation.
//!
//! The canonical server contract (repo `cloud/clients/agents`) is:
//!   GET  /v1/agents            -> { "agents": [Agent, ...] }
//!   GET  /v1/agents/:name      -> AgentDetail
//!   POST /v1/agents/:name/run  -> Run          (body { "input": ... })
//!   GET  /v1/agents/:name/runs -> { "runs": [Run, ...] }
//!
//! Tenant isolation is the gateway-minted `X-Org-Id` derived from the caller's
//! validated IAM owner claim (HIP-0026); this client only sends the bearer JWT
//! via the supplied [`codex_api::SharedAuthProvider`] and never sets the org
//! header itself.

mod api;
mod client;
mod types;

use std::sync::Arc;

pub use api::AgentsBackend;
pub use client::HttpClient;
pub use types::Agent;
pub use types::AgentDetail;
pub use types::ControlBatch;
pub use types::ControlCommand;
pub use types::Run;
pub use types::Session;
pub use types::SessionRegister;

/// Default provider for an unauthenticated client: adds no headers. A real
/// caller replaces it via [`HttpClient::with_auth_provider`].
#[derive(Debug)]
struct Unauthenticated;

impl codex_api::AuthProvider for Unauthenticated {
    fn add_auth_headers(&self, _headers: &mut reqwest::header::HeaderMap) {}
}

pub(crate) fn model_provider_unauthenticated() -> codex_api::SharedAuthProvider {
    Arc::new(Unauthenticated)
}

/// Errors surfaced by the agents backend.
#[derive(Debug, thiserror::Error)]
pub enum AgentsError {
    /// The request could not be sent or the response could not be read.
    #[error("http error: {0}")]
    Http(String),
    /// The server returned a non-success status; the message quotes the server.
    #[error("{0}")]
    Server(String),
    /// The response body could not be decoded into the expected shape.
    #[error("decode error: {0}")]
    Decode(String),
    /// The agent name is not a valid org-unique handle. Rejected client-side so
    /// a dot-segment (`.`/`..`) can never rewrite the request path.
    #[error("invalid agent name {0:?}: expected {expected}", expected = crate::client::NAME_RULE)]
    InvalidName(String),
}

pub type Result<T> = std::result::Result<T, AgentsError>;

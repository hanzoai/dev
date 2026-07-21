use std::future::Future;
use std::pin::Pin;

use crate::Agent;
use crate::AgentDetail;
use crate::Result;
use crate::Run;

type BackendFuture<'a, T> = Pin<Box<dyn Future<Output = Result<T>> + Send + 'a>>;

/// The org-scoped cloud agents registry. Object-safe (boxed futures) so it can
/// be held as `Arc<dyn AgentsBackend>`, matching the sibling cloud-tasks crate.
pub trait AgentsBackend: Send + Sync {
    /// List the calling org's agents (`GET /v1/agents`).
    fn list_agents(&self) -> BackendFuture<'_, Vec<Agent>>;

    /// Fetch one agent with its instructions and recent runs
    /// (`GET /v1/agents/:name`).
    fn get_agent<'a>(&'a self, name: &'a str) -> BackendFuture<'a, AgentDetail>;

    /// Run an agent against `input` and return the recorded run
    /// (`POST /v1/agents/:name/run`).
    fn run_agent<'a>(&'a self, name: &'a str, input: &'a str) -> BackendFuture<'a, Run>;
}

use serde::Deserialize;
use serde::Serialize;

/// A cloud agent as returned by `GET /v1/agents` (the `agentView` contract).
/// `org` is intentionally absent: it is the tenant key and never leaves the
/// server.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Agent {
    pub id: String,
    pub name: String,
    pub model: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub tools: Vec<String>,
    pub status: String,
    #[serde(default)]
    pub runs: u64,
    #[serde(default)]
    pub created_at: String,
    #[serde(default)]
    pub updated_at: String,
}

/// `GET /v1/agents/:name` — an [`Agent`] plus its instructions and recent runs.
/// The server embeds the agent view inline, so the fields are flattened here.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AgentDetail {
    #[serde(flatten)]
    pub agent: Agent,
    #[serde(default)]
    pub instructions: String,
    #[serde(default)]
    pub recent_runs: Vec<Run>,
}

/// A single recorded run (`runView`). Every run reflects an execution that
/// actually happened: `status` is `"ok"` with `output`, or `"error"` with
/// `error` set.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Run {
    pub id: String,
    pub status: String,
    pub model: String,
    #[serde(default)]
    pub input: String,
    #[serde(default)]
    pub output: String,
    #[serde(default)]
    pub error: String,
    #[serde(default)]
    pub duration_ms: i64,
    #[serde(default)]
    pub created_at: String,
}

impl Run {
    /// True when the run completed successfully.
    pub fn is_ok(&self) -> bool {
        self.status == "ok"
    }
}

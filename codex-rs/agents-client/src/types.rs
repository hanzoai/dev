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

/// A live agent SESSION as the cloud registry (`/v1/agents/sessions`) reports it.
/// The CLI registers one per `hanzo code`/`dev` run so the run is watchable in the
/// hanzo.bot playground fleet; `id` is the handle every later call (events, patch,
/// stop) and the `hanzo.bot/sessions/<id>` deep-link use.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Session {
    pub id: String,
    #[serde(default)]
    pub agent: String,
    #[serde(default)]
    pub status: String,
    #[serde(default)]
    pub title: String,
    #[serde(default)]
    pub host: String,
    #[serde(default)]
    pub cwd: String,
    #[serde(default)]
    pub repo: String,
}

/// The fields to register a session with (`POST /v1/agents/sessions`). Only
/// `agent` is required by the server; the rest are optional execution context.
#[derive(Clone, Debug, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionRegister {
    pub agent: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub title: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub host: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub cwd: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub repo: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub target: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub provider: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub account: String,
}

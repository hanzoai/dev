use std::process::Command;
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime};

use anyhow::{anyhow, Context, Result};
use code_browser::global as browser_global;
use code_core::agent_defaults::{default_agent_configs, enabled_agent_model_specs};
use code_core::config::Config;
use code_core::config_types::{AutoDriveSettings, ReasoningEffort};
use code_core::debug_logger::DebugLogger;
use code_core::model_family::{derive_default_model_family, find_family_for_model};
use code_core::{get_openai_tools, OpenAiTool, ToolsConfig};
use code_core::project_doc::read_auto_drive_docs;
use code_core::protocol::SandboxPolicy;
use code_core::slash_commands::get_enabled_agents;
use code_core::{AuthManager, ModelClient, Prompt, ResponseEvent, TextFormat};
use code_core::error::CodexErr;
use code_protocol::models::{ContentItem, ReasoningItemContent, ResponseItem};
use futures::StreamExt;
use reqwest::StatusCode;
use serde::Deserialize;
use serde_json::{self, json, Value};
use tokio_util::sync::CancellationToken;
use tracing::{debug, warn};
use uuid::Uuid;

use crate::app_event::{
    AppEvent,
    AutoCoordinatorStatus,
    AutoObserverStatus,
    AutoObserverTelemetry,
    AutoReviewCommit,
    AutoReviewCommitSource,
    AutoTurnAgentsAction,
    AutoTurnAgentsTiming,
    AutoTurnCliAction,
    AutoTurnCodeReviewAction,
    AutoTurnCrossCheckAction,
};
use crate::app_event_sender::AppEventSender;
use crate::chatwidget::retry::{retry_with_backoff, RetryDecision, RetryError, RetryOptions};
use crate::thread_spawner;
#[cfg(feature = "dev-faults")]
use crate::chatwidget::faults::{fault_to_error, next_fault, FaultScope, InjectedFault};
use code_common::elapsed::format_duration;
use std::fs;
use chrono::{DateTime, Local, Utc};
use rand::Rng;
use super::auto_observer::{
    build_observer_conversation,
    run_observer_once,
    start_auto_observer,
    AutoObserverHandle,
    AutoObserverCommand,
    ObserverOutcome,
    ObserverReason,
    ObserverTrigger,
    summarize_intervention,
};

const RATE_LIMIT_BUFFER: Duration = Duration::from_secs(120);
const RATE_LIMIT_JITTER_MAX: Duration = Duration::from_secs(30);
const MAX_RETRY_ELAPSED: Duration = Duration::from_secs(7 * 24 * 60 * 60);
const MAX_DECISION_RECOVERY_ATTEMPTS: u32 = 3;

#[derive(Debug, thiserror::Error)]
#[error("auto coordinator cancelled")]
struct AutoCoordinatorCancelled;

pub(super) const MODEL_SLUG: &str = "gpt-5";
const SCHEMA_NAME: &str = "auto_coordinator_flow";
pub(super) const CROSS_CHECK_RESTART_BANNER: &str = "Cross-check restart with minimal context";
const COORDINATOR_PROMPT_PATH: &str = "code-rs/core/prompt_coordinator.md";

#[derive(Debug, Clone)]
pub(super) struct AutoCoordinatorHandle {
    pub tx: Sender<AutoCoordinatorCommand>,
    cancel_token: CancellationToken,
}

impl AutoCoordinatorHandle {
    pub fn send(
        &self,
        command: AutoCoordinatorCommand,
    ) -> std::result::Result<(), mpsc::SendError<AutoCoordinatorCommand>> {
        self.tx.send(command)
    }

    pub fn cancel(&self) {
        self.cancel_token.cancel();
    }

}

#[derive(Debug)]
pub(super) enum AutoCoordinatorCommand {
    UpdateConversation(Vec<ResponseItem>),
    ObserverResult(ObserverOutcome),
    Stop,
}

#[derive(Debug, Clone)]
pub(super) struct CrossCheckTurnSnapshot {
    pub cli_prompt: Option<String>,
    pub cli_context: Option<String>,
    pub progress_summary: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum TurnComplexity {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TurnConfig {
    #[serde(default)]
    pub read_only: bool,
    #[serde(default)]
    #[allow(dead_code)]
    pub complexity: Option<TurnComplexity>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum TurnMode {
    Normal,
    SubAgentWrite,
    SubAgentReadOnly,
    Review,
}

impl Default for TurnMode {
    fn default() -> Self {
        Self::Normal
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize, Default)]
pub(crate) struct AgentPreferences {
    #[serde(default)]
    pub prefer_research: bool,
    #[serde(default)]
    pub prefer_planning: bool,
    #[serde(default)]
    pub requested_models: Option<Vec<String>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ReviewTiming {
    PostTurn,
    PreWrite,
    Immediate,
}

impl Default for ReviewTiming {
    fn default() -> Self {
        Self::PostTurn
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub(crate) struct ReviewStrategy {
    #[serde(default)]
    pub timing: ReviewTiming,
    #[serde(default)]
    pub custom_prompt: Option<String>,
    #[serde(default)]
    pub scope_hint: Option<String>,
}

impl Default for ReviewStrategy {
    fn default() -> Self {
        Self {
            timing: ReviewTiming::PostTurn,
            custom_prompt: None,
            scope_hint: None,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TurnDescriptor {
    #[serde(default)]
    pub mode: TurnMode,
    #[serde(default)]
    pub read_only: bool,
    #[serde(default)]
    pub complexity: Option<TurnComplexity>,
    #[serde(default)]
    pub agent_preferences: Option<AgentPreferences>,
    #[serde(default)]
    pub review_strategy: Option<ReviewStrategy>,
}

impl Default for TurnDescriptor {
    fn default() -> Self {
        Self {
            mode: TurnMode::Normal,
            read_only: false,
            complexity: None,
            agent_preferences: None,
            review_strategy: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::anyhow;
    use code_core::agent_defaults::DEFAULT_AGENT_NAMES;
    use serde_json::json;

    #[test]
    fn turn_descriptor_defaults_to_normal_mode() {
        let value = json!({});
        let descriptor: TurnDescriptor = serde_json::from_value(value).unwrap();
        assert_eq!(descriptor.mode, TurnMode::Normal);
        assert!(!descriptor.read_only);
        assert!(descriptor.complexity.is_none());
        assert!(descriptor.agent_preferences.is_none());
        assert!(descriptor.review_strategy.is_none());
    }

    #[test]
    fn schema_includes_cli_agents_code_review_and_cross_check() {
        let active_agents = vec![
            "codex-plan".to_string(),
            "codex-research".to_string(),
        ];
        let schema = build_schema(&active_agents, SchemaFeatures::default());
        let props = schema
            .get("properties")
            .and_then(|v| v.as_object())
            .expect("schema properties");
        assert!(props.contains_key("cli"), "cli property missing");
        assert!(props.contains_key("agents"), "agents property missing");
        assert!(props.contains_key("code_review"), "code_review property missing");
        assert!(props.contains_key("cross_check"), "cross_check property missing");

        let cli_required = props
            .get("cli")
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("required"))
            .and_then(|v| v.as_array())
            .expect("cli required");
        assert!(cli_required.contains(&json!("prompt")));
        assert!(cli_required.contains(&json!("context")));

        let agents_obj = props
            .get("agents")
            .and_then(|v| v.as_object())
            .expect("agents schema object");
        let agents_required = agents_obj
            .get("required")
            .and_then(|v| v.as_array())
            .expect("agents required");
        assert!(agents_required.contains(&json!("timing")));
        assert!(agents_required.contains(&json!("list")));
        assert!(!agents_required.contains(&json!("models")));

        let list_items_schema = agents_obj
            .get("properties")
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("list"))
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("items"))
            .and_then(|v| v.as_object())
            .expect("agents.list items");
        let item_props = list_items_schema
            .get("properties")
            .and_then(|v| v.as_object())
            .expect("agents.list item properties");
        let models_schema = item_props
            .get("models")
            .and_then(|v| v.as_object())
            .expect("agents.list item models schema");
        assert_eq!(models_schema.get("type"), Some(&json!("array")));
        let enum_values = models_schema
            .get("items")
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("enum"))
            .and_then(|v| v.as_array())
            .expect("models enum values");
        let expected_enum: Vec<Value> = active_agents
            .iter()
            .map(|name| Value::String(name.clone()))
            .collect();
        assert_eq!(*enum_values, expected_enum);

        let review_required = props
            .get("code_review")
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("required"))
            .and_then(|v| v.as_array())
            .expect("review required");
        assert!(review_required.contains(&json!("source")));
        assert!(review_required.contains(&json!("sha")));
        assert!(review_required.contains(&json!("summary")));

        let cross_check_props = props
            .get("cross_check")
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("properties"))
            .and_then(|v| v.as_object())
            .expect("cross_check properties");
        assert!(cross_check_props.contains_key("summary"));
        assert!(cross_check_props.contains_key("focus"));
    }

    #[test]
    fn schema_defaults_to_builtin_agents_enum() {
        let schema = build_schema(
            &DEFAULT_AGENT_NAMES
                .iter()
                .map(|name| (*name).to_string())
                .collect::<Vec<_>>(),
            SchemaFeatures::default(),
        );
        let props = schema
            .get("properties")
            .and_then(|v| v.as_object())
            .expect("schema properties");
        let agents_obj = props
            .get("agents")
            .and_then(|v| v.as_object())
            .expect("agents schema");
        let item_enum = agents_obj
            .get("properties")
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("list"))
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("items"))
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("properties"))
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("models"))
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("items"))
            .and_then(|v| v.as_object())
            .and_then(|obj| obj.get("enum"))
            .and_then(|v| v.as_array())
            .expect("models enum");
        let expected: Vec<Value> = DEFAULT_AGENT_NAMES
            .iter()
            .map(|name| Value::String((*name).to_string()))
            .collect();
        assert_eq!(*item_enum, expected);
    }

    #[test]
    fn schema_omits_agents_when_disabled() {
        let active_agents = vec!["codex-plan".to_string()];
        let schema = build_schema(
            &active_agents,
            SchemaFeatures {
                include_agents: false,
                ..SchemaFeatures::default()
            },
        );
        let props = schema
            .get("properties")
            .and_then(|v| v.as_object())
            .expect("schema properties");
        assert!(!props.contains_key("agents"));
        let required = schema
            .get("required")
            .and_then(|v| v.as_array())
            .expect("required array");
        assert!(!required.contains(&json!("agents")));
    }

    #[test]
    fn schema_omits_code_review_when_disabled() {
        let schema = build_schema(
            &[],
            SchemaFeatures {
                include_review: false,
                ..SchemaFeatures::default()
            },
        );
        let props = schema
            .get("properties")
            .and_then(|v| v.as_object())
            .expect("schema properties");
        assert!(!props.contains_key("code_review"));
        let required = schema
            .get("required")
            .and_then(|v| v.as_array())
            .expect("required array");
        assert!(!required.contains(&json!("code_review")));
    }

    #[test]
    fn schema_omits_cross_check_when_disabled() {
        let schema = build_schema(
            &[],
            SchemaFeatures {
                include_cross_check: false,
                ..SchemaFeatures::default()
            },
        );
        let props = schema
            .get("properties")
            .and_then(|v| v.as_object())
            .expect("schema properties");
        assert!(!props.contains_key("cross_check"));
        let required = schema
            .get("required")
            .and_then(|v| v.as_array())
            .expect("required array");
        assert!(!required.contains(&json!("cross_check")));
    }

    #[test]
    fn parse_decision_new_schema() {
        let raw = r#"{
            "finish_status": "continue",
            "progress": {"past": "Ran smoke tests", "current": "Dispatching fix"},
            "cli": {"prompt": "Apply the patch for the failing test", "context": "tests/failing.rs"},
            "agents": {
                "timing": "blocking",
                "list": [
                    {"prompt": "Draft alternative fix", "write": false, "context": "Consider module B", "models": ["codex-plan"]}
                ]
            },
            "code_review": {"source": "staged", "summary": "Pre-merge sanity"},
            "cross_check": {"summary": "Verify database wiring", "focus": "CRUD paths"}
        }"#;

        let (decision, _) = parse_decision(raw).expect("parse new schema decision");
        assert_eq!(decision.status, AutoCoordinatorStatus::Continue);
        assert_eq!(decision.progress_past.as_deref(), Some("Ran smoke tests"));
        assert_eq!(decision.progress_current.as_deref(), Some("Dispatching fix"));

        let cli = decision.cli.expect("cli action expected");
        assert_eq!(cli.prompt, "Apply the patch for the failing test");
        assert_eq!(cli.context.as_deref(), Some("tests/failing.rs"));

        assert_eq!(
            decision.agents_timing,
            Some(AutoTurnAgentsTiming::Blocking)
        );
        assert_eq!(decision.agents.len(), 1);
        let agent = &decision.agents[0];
        assert_eq!(agent.prompt, "Draft alternative fix");
        assert!(!agent.write);
        assert_eq!(
            agent.models,
            Some(vec!["codex-plan".to_string()])
        );

        let review = decision
            .code_review
            .expect("code_review action expected");
        assert!(matches!(review.commit.source, AutoReviewCommitSource::Staged));
        assert_eq!(review.commit.summary.as_deref(), Some("Pre-merge sanity"));
        let cross_check = decision.cross_check.expect("cross_check action expected");
        assert_eq!(cross_check.summary.as_deref(), Some("Verify database wiring"));
        assert_eq!(cross_check.focus.as_deref(), Some("CRUD paths"));
    }

    #[test]
    fn parse_decision_new_schema_array_backcompat() {
        let raw = r#"{
            "finish_status": "continue",
            "progress": {"past": "Outlined fix", "current": "Running tests"},
            "cli": {"prompt": "Run cargo test", "context": null},
            "agents": [
                {"prompt": "Investigate benchmark", "write": false}
            ]
        }"#;

        let (decision, _) = parse_decision(raw).expect("parse array-style agents");
        assert_eq!(decision.status, AutoCoordinatorStatus::Continue);
        assert!(decision.cli.is_some());
        assert_eq!(decision.agents.len(), 1);
        assert!(decision.agents_timing.is_none());
    }

    #[test]
    fn parse_decision_legacy_schema() {
        let raw = r#"{
            "finish_status": "continue",
            "progress_past": "Drafted fix",
            "progress_current": "Running unit tests",
            "cli_prompt": "Run cargo test --package core",
            "cli_context": "Focus on flaky suite",
            "review_commit": {
                "source": "staged",
                "sha": null,
                "summary": "Smoke diff"
            }
        }"#;

        let (decision, _) = parse_decision(raw).expect("parse legacy decision");
        assert_eq!(decision.status, AutoCoordinatorStatus::Continue);
        assert_eq!(decision.progress_past.as_deref(), Some("Drafted fix"));
        assert_eq!(decision.progress_current.as_deref(), Some("Running unit tests"));

        let cli = decision.cli.expect("cli action expected");
        assert_eq!(cli.prompt, "Run cargo test --package core");
        assert_eq!(cli.context.as_deref(), Some("Focus on flaky suite"));

        assert!(decision.agents.is_empty());
        assert!(decision.agents_timing.is_none());
        let review = decision
            .code_review
            .expect("code_review action expected");
        assert!(matches!(review.commit.source, AutoReviewCommitSource::Staged));
    }

    #[test]
    fn classify_missing_cli_prompt_is_recoverable() {
        let err = anyhow!("model response missing cli prompt for continue");
        let info = classify_recoverable_decision_error(&err).expect("recoverable error");
        assert!(info.summary.contains("missing CLI prompt"));
        assert!(
            info
                .guidance
                .as_ref()
                .expect("guidance")
                .contains("cli.prompt")
        );
    }

    #[test]
    fn classify_empty_field_is_recoverable() {
        let err = anyhow!("agents[*].prompt is empty");
        let info = classify_recoverable_decision_error(&err).expect("recoverable error");
        assert!(info.summary.contains("agents[*].prompt"));
        assert!(info
            .guidance
            .as_ref()
            .expect("guidance")
            .contains("agents[*].prompt"));
    }

    #[test]
    fn push_unique_guidance_trims_and_dedupes() {
        let mut guidance = vec!["Keep CLI prompts short".to_string()];
        push_unique_guidance(&mut guidance, "  keep cli prompts short  ");
        assert_eq!(guidance.len(), 1, "duplicate hint should not be added");
        push_unique_guidance(&mut guidance, "Respond with JSON only");
        assert_eq!(guidance.len(), 2);
        assert!(guidance.iter().any(|hint| hint == "Respond with JSON only"));
    }
}

#[derive(Debug, Deserialize)]
struct CoordinatorDecisionNew {
    finish_status: String,
    progress: ProgressPayload,
    #[serde(default)]
    cli: Option<CliPayload>,
    #[serde(default)]
    agents: Option<AgentsField>,
    #[serde(default)]
    code_review: Option<ReviewPayload>,
    #[serde(default)]
    cross_check: Option<CrossCheckPayload>,
}

#[derive(Debug, Deserialize)]
struct ProgressPayload {
    #[serde(default)]
    past: Option<String>,
    #[serde(default)]
    current: Option<String>,
}

#[derive(Debug, Deserialize)]
struct CliPayload {
    prompt: String,
    #[serde(default)]
    context: Option<String>,
}

#[derive(Debug, Deserialize)]
struct AgentPayload {
    prompt: String,
    #[serde(default)]
    context: Option<String>,
    #[serde(default)]
    write: bool,
    #[serde(default)]
    models: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum AgentsField {
    List(Vec<AgentPayload>),
    Object(AgentsPayload),
}

#[derive(Debug, Deserialize)]
struct AgentsPayload {
    #[serde(default)]
    timing: Option<AgentsTimingValue>,
    #[serde(default)]
    models: Option<Vec<String>>,
    #[serde(
        default,
        alias = "list",
        alias = "agents",
        alias = "entries",
        alias = "requests"
    )]
    requests: Vec<AgentPayload>,
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "snake_case")]
enum AgentsTimingValue {
    Parallel,
    Blocking,
}

impl From<AgentsTimingValue> for AutoTurnAgentsTiming {
    fn from(value: AgentsTimingValue) -> Self {
        match value {
            AgentsTimingValue::Parallel => AutoTurnAgentsTiming::Parallel,
            AgentsTimingValue::Blocking => AutoTurnAgentsTiming::Blocking,
        }
    }
}

#[derive(Debug, Deserialize)]
struct ReviewPayload {
    source: String,
    #[serde(default)]
    sha: Option<String>,
    #[serde(default)]
    summary: Option<String>,
}

#[derive(Debug, Deserialize)]
struct CrossCheckPayload {
    #[serde(default)]
    summary: Option<String>,
    #[serde(default)]
    focus: Option<String>,
}

#[derive(Debug, Deserialize)]
struct CoordinatorDecisionLegacy {
    finish_status: String,
    #[serde(default)]
    progress_past: Option<String>,
    #[serde(default)]
    progress_current: Option<String>,
    #[serde(default)]
    cli_context: Option<String>,
    #[serde(default)]
    cli_prompt: Option<String>,
    #[serde(default)]
    review_commit: Option<ReviewCommitPayloadLegacy>,
}

#[derive(Debug, Deserialize)]
struct ReviewCommitPayloadLegacy {
    source: String,
    #[serde(default)]
    sha: Option<String>,
    #[serde(default)]
    summary: Option<String>,
}

struct ParsedCoordinatorDecision {
    status: AutoCoordinatorStatus,
    progress_past: Option<String>,
    progress_current: Option<String>,
    cli: Option<CliAction>,
    agents_timing: Option<AutoTurnAgentsTiming>,
    agents: Vec<AgentAction>,
    code_review: Option<CodeReviewAction>,
    cross_check: Option<CrossCheckAction>,
    response_items: Vec<ResponseItem>,
}

#[derive(Debug, Clone)]
struct CliAction {
    prompt: String,
    context: Option<String>,
}

#[derive(Debug, Clone)]
struct AgentAction {
    prompt: String,
    context: Option<String>,
    write: bool,
    models: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
struct CodeReviewAction {
    commit: AutoReviewCommit,
}

#[derive(Debug, Clone)]
struct CrossCheckAction {
    summary: Option<String>,
    focus: Option<String>,
    forced: bool,
}

pub(super) fn start_auto_coordinator(
    app_event_tx: AppEventSender,
    goal_text: String,
    conversation: Vec<ResponseItem>,
    config: Config,
    debug_enabled: bool,
    observer_cadence: u32,
) -> Result<AutoCoordinatorHandle> {
    let (cmd_tx, cmd_rx) = mpsc::channel();
    let thread_tx = cmd_tx.clone();
    let loop_tx = cmd_tx.clone();
    let cancel_token = CancellationToken::new();
    let thread_cancel = cancel_token.clone();

    if thread_spawner::spawn_lightweight("auto-coordinator", move || {
        if let Err(err) = run_auto_loop(
            app_event_tx,
            goal_text,
            conversation,
            config,
            cmd_rx,
            loop_tx,
            debug_enabled,
            observer_cadence,
            thread_cancel,
        ) {
            tracing::error!("auto coordinator loop error: {err:#}");
        }
    })
    .is_none()
    {
        tracing::error!("auto coordinator spawn rejected: background thread limit reached");
        return Err(anyhow!("auto coordinator worker unavailable"));
    }

    Ok(AutoCoordinatorHandle {
        tx: thread_tx,
        cancel_token,
    })
}

fn run_auto_loop(
    app_event_tx: AppEventSender,
    goal_text: String,
    initial_conversation: Vec<ResponseItem>,
    config: Config,
    cmd_rx: Receiver<AutoCoordinatorCommand>,
    cmd_tx: Sender<AutoCoordinatorCommand>,
    debug_enabled: bool,
    observer_cadence: u32,
    cancel_token: CancellationToken,
) -> Result<()> {
    let preferred_auth = if config.using_chatgpt_auth {
        code_protocol::mcp_protocol::AuthMode::ChatGPT
    } else {
        code_protocol::mcp_protocol::AuthMode::ApiKey
    };
    let code_home = config.code_home.clone();
    let responses_originator_header = config.responses_originator_header.clone();
    let auth_mgr = AuthManager::shared_with_mode_and_originator(
        code_home,
        preferred_auth,
        responses_originator_header,
    );
    let model_provider = config.model_provider.clone();
    let model_reasoning_summary = config.model_reasoning_summary;
    let model_text_verbosity = config.model_text_verbosity;
    let sandbox_policy = config.sandbox_policy.clone();
    let config = Arc::new(config);
    let active_agent_names = get_enabled_agents(&config.agents);
    let client = Arc::new(ModelClient::new(
        config.clone(),
        Some(auth_mgr),
        None,
        model_provider,
        ReasoningEffort::Medium,
        model_reasoning_summary,
        model_text_verbosity,
        Uuid::new_v4(),
        Arc::new(Mutex::new(
            DebugLogger::new(debug_enabled)
                .unwrap_or_else(|_| DebugLogger::new(false).expect("debug logger")),
        )),
    ));

    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("creating runtime for auto coordinator")?;

    let browser_enabled = runtime.block_on(async {
        browser_global::get_browser_manager().await.is_some()
    });

    let mut tools_config = ToolsConfig::new(
        &config.model_family,
        config.approval_policy,
        config.sandbox_policy.clone(),
        config.include_plan_tool,
        config.include_apply_patch_tool,
        config.tools_web_search_request,
        config.use_experimental_streamable_shell_tool,
        config.include_view_image_tool,
    );
    tools_config.web_search_allowed_domains = config.tools_web_search_allowed_domains.clone();

    let mut agent_models: Vec<String> = if config.agents.is_empty() {
        default_agent_configs()
            .into_iter()
            .filter(|cfg| cfg.enabled)
            .map(|cfg| cfg.name)
            .collect()
    } else {
        get_enabled_agents(&config.agents)
    };
    if agent_models.is_empty() {
        agent_models = enabled_agent_model_specs()
            .into_iter()
            .map(|spec| spec.slug.to_string())
            .collect();
    }
    agent_models.sort_by(|a, b| a.to_ascii_lowercase().cmp(&b.to_ascii_lowercase()));
    agent_models.dedup_by(|a, b| a.eq_ignore_ascii_case(b));
    tools_config.set_agent_models(agent_models);
    let cross_check_tools: Vec<OpenAiTool> = get_openai_tools(&tools_config, None, browser_enabled, true);

    let auto_instructions = match runtime.block_on(read_auto_drive_docs(config.as_ref())) {
        Ok(Some(text)) => {
            let trimmed = text.trim().to_string();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed)
            }
        }
        Ok(None) => None,
        Err(err) => {
            warn!("failed to read AUTO_AGENTS.md instructions: {err:#}");
            None
        }
    };
    let sandbox_label = if matches!(sandbox_policy, SandboxPolicy::DangerFullAccess) {
        "full access"
    } else {
        "limited sandbox"
    };
    let environment_details = format_environment_details(sandbox_label);
    let coordinator_prompt = read_coordinator_prompt(config.as_ref());
    let (base_developer_intro, primary_goal_message) =
        build_developer_message(&goal_text, &environment_details, coordinator_prompt.as_deref());
    let schema_features = SchemaFeatures::from_auto_settings(&config.tui.auto_drive);
    let include_agents = schema_features.include_agents;
    let include_review = schema_features.include_review;
    let include_cross_check = schema_features.include_cross_check;
    let schema = build_schema(&active_agent_names, schema_features);
    let platform = std::env::consts::OS;
    debug!("[Auto coordinator] starting: goal={goal_text} platform={platform}");

    let mut pending_conversation = Some(initial_conversation);
    let mut stopped = false;
    let mut requests_completed: u64 = 0;
    let mut consecutive_decision_failures: u32 = 0;
    let mut observer_guidance: Vec<String> = Vec::new();
    let mut observer_telemetry = AutoObserverTelemetry::default();
    let mut last_cli_context: Option<String> = None;
    let mut last_cli_prompt: Option<String> = None;
    let mut last_progress_summary: Option<String> = None;
    let mut observer_handle = match start_auto_observer(client.clone(), observer_cadence, cmd_tx.clone()) {
        Ok(handle) => Some(handle),
        Err(err) => {
            tracing::error!("failed to start auto observer: {err:#}");
            None
        }
    };
    let observer_cadence_interval = if observer_cadence == 0 {
        None
    } else {
        observer_handle
            .as_ref()
            .map(|handle| handle.cadence() as u64)
    };

    loop {
        if stopped {
            break;
        }

        if let Some(conv) = pending_conversation.take() {
            if cancel_token.is_cancelled() {
                stopped = true;
                continue;
            }

            let conv_for_observer = conv.clone();
            let developer_intro =
                compose_developer_intro(&base_developer_intro, &observer_guidance);
            match request_coordinator_decision(
                &runtime,
                client.as_ref(),
                developer_intro.as_str(),
                &primary_goal_message,
                &schema,
                conv,
                auto_instructions.as_deref(),
                &app_event_tx,
                &cancel_token,
            ) {
                Ok(ParsedCoordinatorDecision {
                    status,
                    progress_past,
                    progress_current,
                    cli,
                    mut agents_timing,
                    mut agents,
                    mut code_review,
                    mut cross_check,
                    mut response_items,
                }) => {
                    if !include_agents {
                        agents_timing = None;
                        agents.clear();
                    }
                    if !include_review {
                        code_review = None;
                    }
                    if !include_cross_check {
                        cross_check = None;
                    }
                    consecutive_decision_failures = 0;
                    let (turn_cli_prompt, turn_cli_context) = if let Some(cli_action) = &cli {
                        let prompt = Some(cli_action.prompt.clone());
                        let context = cli_action
                            .context
                            .as_ref()
                            .map(|ctx| ctx.trim().to_string())
                            .filter(|ctx| !ctx.is_empty());
                        (prompt, context)
                    } else {
                        (None, None)
                    };
                    last_cli_prompt = turn_cli_prompt.clone();
                    last_cli_context = turn_cli_context.clone();

                    let mut latest_summary = progress_current.clone();
                    if latest_summary.is_none() {
                        latest_summary = progress_past.clone();
                    }
                    if let Some(summary_text) = latest_summary.clone() {
                        last_progress_summary = Some(summary_text);
                    } else {
                        last_progress_summary = None;
                    }

                    let turn_snapshot = CrossCheckTurnSnapshot {
                        cli_prompt: turn_cli_prompt.clone(),
                        cli_context: turn_cli_context.clone(),
                        progress_summary: latest_summary.clone(),
                    };

                    let cli_prompt_for_observer = turn_cli_prompt.as_deref();

                    if matches!(status, AutoCoordinatorStatus::Continue) {
                        if include_cross_check {
                            if let Some(cross) = cross_check.clone() {
                                dispatch_cross_check(
                                    observer_handle.as_ref(),
                                    &runtime,
                                    client.clone(),
                                    &cmd_tx,
                                    conv_for_observer.clone(),
                                    goal_text.clone(),
                                    environment_details.clone(),
                                    cross.summary.clone(),
                                    cross.focus.clone(),
                                    false,
                                    Some(turn_snapshot.clone()),
                                    cross_check_tools.clone(),
                                );
                            }
                        }

                        if let (Some(handle), Some(cadence)) =
                            (observer_handle.as_ref(), observer_cadence_interval)
                        {
                            if should_trigger_observer(requests_completed, cadence) {
                                let conversation = build_observer_conversation(
                                    conv_for_observer,
                                    cli_prompt_for_observer,
                                );
                                let trigger = ObserverTrigger {
                                    conversation,
                                    goal_text: goal_text.clone(),
                                    environment_details: environment_details.clone(),
                                    reason: ObserverReason::Cadence,
                                    turn_snapshot: None,
                                    tools: Vec::new(),
                                };
                                if handle.tx.send(AutoObserverCommand::Trigger(trigger)).is_err() {
                                    warn!("failed to trigger auto observer");
                                }
                            }
                        }

                        let cross_check_event = if include_cross_check {
                            cross_check
                                .as_ref()
                                .map(|action| cross_check_action_to_event(action))
                        } else {
                            None
                        };
                        let event = AppEvent::AutoCoordinatorDecision {
                            status,
                            progress_past,
                            progress_current,
                            cli: cli.as_ref().map(cli_action_to_event),
                            agents_timing,
                            agents: agents.iter().map(agent_action_to_event).collect(),
                            code_review: code_review.as_ref().map(code_review_action_to_event),
                            cross_check: cross_check_event,
                            transcript: std::mem::take(&mut response_items),
                        };
                        app_event_tx.send(event);
                        continue;
                    }

                    if include_cross_check {
                        let observer_conversation =
                            build_observer_conversation(conv_for_observer.clone(), None);
                        let final_summary = cross_check
                            .as_ref()
                            .and_then(|data| data.summary.clone());
                        let final_focus = cross_check
                            .as_ref()
                            .and_then(|data| data.focus.clone());

                        let validation_result = run_final_cross_check(
                            &runtime,
                            client.clone(),
                            observer_conversation,
                            &goal_text,
                            &environment_details,
                            final_summary,
                            final_focus,
                            Some(turn_snapshot),
                            cross_check_tools.clone(),
                        );

                        if let Ok((observer_status, replace_message, additional_instructions)) =
                            &validation_result
                        {
                            let telemetry = AutoObserverTelemetry {
                                trigger_count: observer_telemetry.trigger_count.saturating_add(1),
                                last_status: *observer_status,
                                last_intervention: summarize_intervention(
                                    replace_message.as_deref(),
                                    additional_instructions.as_deref(),
                                ),
                            };
                            observer_telemetry = telemetry.clone();
                            let observer_event = AppEvent::AutoObserverReport {
                                status: *observer_status,
                                telemetry,
                                replace_message: replace_message.clone(),
                                additional_instructions: additional_instructions.clone(),
                            };
                            app_event_tx.send(observer_event);

                            if matches!(observer_status, AutoObserverStatus::Failing) {
                                if let Some(instr) = additional_instructions.as_deref() {
                                    push_unique_guidance(&mut observer_guidance, instr);
                                }
                                pending_conversation = Some(conv_for_observer);
                                continue;
                            }
                        } else if let Err(err) = validation_result {
                            warn!("final observer validation failed: {err:#}");
                        }
                    }

                    let event = AppEvent::AutoCoordinatorDecision {
                        status,
                        progress_past,
                        progress_current,
                        cli: cli.as_ref().map(cli_action_to_event),
                        agents_timing,
                        agents: agents.iter().map(agent_action_to_event).collect(),
                        code_review: code_review.as_ref().map(code_review_action_to_event),
                        cross_check: None,
                        transcript: response_items,
                    };
                    app_event_tx.send(event);
                    stopped = true;
                    continue;
                }
                Err(err) => {
                    if err.downcast_ref::<AutoCoordinatorCancelled>().is_some() {
                        stopped = true;
                        continue;
                    }
                    if let Some(recoverable) = classify_recoverable_decision_error(&err) {
                        consecutive_decision_failures =
                            consecutive_decision_failures.saturating_add(1);
                        if consecutive_decision_failures <= MAX_DECISION_RECOVERY_ATTEMPTS {
                            let attempt = consecutive_decision_failures;
                            warn!(
                                "auto coordinator decision validation failed (attempt {}/{}): {:#}",
                                attempt,
                                MAX_DECISION_RECOVERY_ATTEMPTS,
                                err
                            );
                            if let Some(hint) = recoverable.guidance.as_deref() {
                                push_unique_guidance(&mut observer_guidance, hint);
                            }
                            let message = format!(
                                "Coordinator response invalid (attempt {attempt}/{MAX_DECISION_RECOVERY_ATTEMPTS}): {}. Retrying…",
                                recoverable.summary
                            );
                            let _ = app_event_tx.send(AppEvent::AutoCoordinatorThinking {
                                delta: message,
                                summary_index: None,
                            });
                            pending_conversation = Some(conv_for_observer);
                            continue;
                        }
                        warn!(
                            "auto coordinator validation retry limit exceeded after {} attempts: {:#}",
                            MAX_DECISION_RECOVERY_ATTEMPTS,
                            err
                        );
                    }
                    consecutive_decision_failures = 0;
                    let event = AppEvent::AutoCoordinatorDecision {
                        status: AutoCoordinatorStatus::Failed,
                        progress_past: None,
                        progress_current: Some(format!("Coordinator error: {err}")),
                        cli: None,
                        agents_timing: None,
                        agents: Vec::new(),
                        code_review: None,
                        cross_check: None,
                        transcript: Vec::new(),
                    };
                    app_event_tx.send(event);
                    stopped = true;
                    continue;
                }
            }
        }

        match cmd_rx.recv() {
            Ok(AutoCoordinatorCommand::UpdateConversation(conv)) => {
                requests_completed = requests_completed.saturating_add(1);
                consecutive_decision_failures = 0;
                pending_conversation = Some(conv);
            }
            Ok(AutoCoordinatorCommand::ObserverResult(outcome)) => {
                let ObserverOutcome {
                    status,
                    replace_message,
                    additional_instructions,
                    telemetry,
                    reason,
                    conversation: _conversation,
                    turn_snapshot,
                } = outcome;

                if let Some(instr) = additional_instructions.as_deref() {
                    push_unique_guidance(&mut observer_guidance, instr);
                }

                observer_telemetry = telemetry.clone();
                let event = AppEvent::AutoObserverReport {
                    status,
                    telemetry,
                    replace_message: replace_message.clone(),
                    additional_instructions: additional_instructions.clone(),
                };
                app_event_tx.send(event);

                if let ObserverReason::CrossCheck { forced: _, summary, focus } = reason {
                    if matches!(status, AutoObserverStatus::Failing) {
                        let mut restart_prompt = String::new();
                        let append_candidate = |target: &mut String, candidate: Option<&str>| {
                            if let Some(text) = candidate {
                                let trimmed = text.trim();
                                if trimmed.is_empty() {
                                    return;
                                }
                                if !target.is_empty() {
                                    target.push_str("\n\n");
                                }
                                target.push_str(trimmed);
                            }
                        };
                        let snapshot_prompt = turn_snapshot
                            .as_ref()
                            .and_then(|snapshot| snapshot.cli_prompt.as_deref());
                        let snapshot_context = turn_snapshot
                            .as_ref()
                            .and_then(|snapshot| snapshot.cli_context.as_deref());
                        let snapshot_summary = turn_snapshot
                            .as_ref()
                            .and_then(|snapshot| snapshot.progress_summary.as_deref());

                        append_candidate(&mut restart_prompt, replace_message.as_deref());

                        let had_replacement = replace_message
                            .as_deref()
                            .map(|text| !text.trim().is_empty())
                            .unwrap_or(false);

                        if !had_replacement {
                            append_candidate(
                                &mut restart_prompt,
                                snapshot_prompt.or_else(|| last_cli_prompt.as_deref()),
                            );
                        }

                        append_candidate(
                            &mut restart_prompt,
                            snapshot_summary.or_else(|| last_progress_summary.as_deref()),
                        );

                        let mut context_sections: Vec<String> = Vec::new();
                        if let Some(context_text) = snapshot_context
                            .or_else(|| last_cli_context.as_deref())
                            .map(str::trim)
                            .filter(|text| !text.is_empty())
                        {
                            context_sections.push(format!("CLI context:\n{}", context_text));
                        }
                        if had_replacement {
                            if let Some(previous_prompt) = snapshot_prompt
                                .or_else(|| last_cli_prompt.as_deref())
                                .map(str::trim)
                                .filter(|text| !text.is_empty())
                            {
                                context_sections.push(format!("Previous CLI prompt:\n{}", previous_prompt));
                            }
                        }
                        if !context_sections.is_empty() {
                            if !restart_prompt.is_empty() {
                                restart_prompt.push_str("\n\n");
                            }
                            restart_prompt.push_str(&context_sections.join("\n\n"));
                        }

                        append_candidate(&mut restart_prompt, summary.as_deref());
                        append_candidate(&mut restart_prompt, focus.as_deref());

                        let restart_prompt = if restart_prompt.is_empty() {
                            None
                        } else {
                            Some(restart_prompt)
                        };

                        let mut guidance_parts: Vec<String> = Vec::new();
                        if let Some(instr) = additional_instructions
                            .as_deref()
                            .map(str::trim)
                            .filter(|s| !s.is_empty())
                        {
                            guidance_parts.push(instr.to_string());
                        }
                        if let Some(summary_text) = summary
                            .as_deref()
                            .map(str::trim)
                            .filter(|s| !s.is_empty())
                        {
                            guidance_parts.push(format!("Cross-check summary: {summary_text}"));
                        }
                        if let Some(focus_text) = focus
                            .as_deref()
                            .map(str::trim)
                            .filter(|s| !s.is_empty())
                        {
                            guidance_parts.push(format!("Areas to inspect: {focus_text}"));
                        }
                        let guidance_bundle = if guidance_parts.is_empty() {
                            None
                        } else {
                            Some(guidance_parts.join("\n\n"))
                        };

                        let compact_conversation = build_compact_conversation_seed(
                            &goal_text,
                            restart_prompt.as_ref().map(|text| text.as_str()),
                            guidance_bundle.as_deref(),
                        );

                        pending_conversation = Some(compact_conversation);
                        requests_completed = requests_completed.saturating_sub(1);
                        consecutive_decision_failures = 0;
                        last_cli_prompt = None;
                        last_cli_context = None;
                        last_progress_summary = None;
                        let _ = app_event_tx.send(AppEvent::AutoCoordinatorThinking {
                            delta: CROSS_CHECK_RESTART_BANNER.to_string(),
                            summary_index: None,
                        });
                        continue;
                    }
                }
            }
            Ok(AutoCoordinatorCommand::Stop) | Err(_) => {
                stopped = true;
            }
        }
    }

    if let Some(handle) = observer_handle.take() {
        let _ = handle.tx.send(AutoObserverCommand::Stop);
    }

    Ok(())
}

fn compose_developer_intro(base: &str, guidance: &[String]) -> String {
    if guidance.is_empty() {
        return base.to_string();
    }

    let mut intro = String::with_capacity(base.len() + guidance.len() * 64);
    intro.push_str(base);
    intro.push_str("\n\n**Observer Guidance**\n");
    for hint in guidance {
        let trimmed = hint.trim();
        if trimmed.is_empty() {
            continue;
        }
        intro.push_str("- ");
        intro.push_str(trimmed);
        intro.push('\n');
    }
    intro
}

pub(super) fn build_compact_conversation_seed(
    goal: &str,
    last_prompt: Option<&str>,
    guidance: Option<&str>,
) -> Vec<ResponseItem> {
    let mut items: Vec<ResponseItem> = Vec::with_capacity(2);

    let trimmed_goal = goal.trim();
    let mut developer_message = format!(
        "You are resuming Auto Drive with minimal context.\nPrimary goal:\n{}",
        trimmed_goal
    );
    if let Some(guidance_text) = guidance
        .map(str::trim)
        .filter(|text| !text.is_empty())
    {
        developer_message.push_str("\n\nCross-check guidance:\n");
        developer_message.push_str(guidance_text);
    }
    items.push(make_message("developer", developer_message));

    let user_prompt = last_prompt
        .map(str::trim)
        .filter(|text| !text.is_empty())
        .map(|text| text.to_string())
        .unwrap_or_else(|| {
            "Restart the plan and verify end-to-end behaviour satisfies the primary goal and the guidance above."
                .to_string()
        });
    items.push(make_message("user", user_prompt));

    items
}

fn dispatch_cross_check(
    observer_handle: Option<&AutoObserverHandle>,
    runtime: &tokio::runtime::Runtime,
    client: Arc<ModelClient>,
    coordinator_tx: &Sender<AutoCoordinatorCommand>,
    conversation: Vec<ResponseItem>,
    goal_text: String,
    environment_details: String,
    summary: Option<String>,
    focus: Option<String>,
    forced: bool,
    turn_snapshot: Option<CrossCheckTurnSnapshot>,
    tools: Vec<OpenAiTool>,
) {
    let reason = ObserverReason::CrossCheck {
        forced,
        summary: summary.clone(),
        focus: focus.clone(),
    };
    let trigger = ObserverTrigger {
        conversation: conversation.clone(),
        goal_text,
        environment_details,
        reason: reason.clone(),
        turn_snapshot: turn_snapshot.clone(),
        tools: tools.clone(),
    };

    if let Some(handle) = observer_handle {
        if handle.tx.send(AutoObserverCommand::Trigger(trigger.clone())).is_ok() {
            return;
        }
    }

    match run_observer_once(runtime, client, trigger.clone()) {
        Ok((status, replace_message, additional_instructions)) => {
            let outcome = ObserverOutcome {
                status,
                replace_message,
                additional_instructions,
                telemetry: AutoObserverTelemetry::default(),
                reason,
                conversation,
                turn_snapshot,
            };
            let _ = coordinator_tx.send(AutoCoordinatorCommand::ObserverResult(outcome));
        }
        Err(err) => {
            warn!("cross-check fallback evaluation failed: {err:#}");
        }
    }
}

fn should_trigger_observer(requests_completed: u64, cadence: u64) -> bool {
    cadence != 0 && requests_completed > 0 && requests_completed % cadence == 0
}

fn run_final_cross_check(
    runtime: &tokio::runtime::Runtime,
    client: Arc<ModelClient>,
    conversation: Vec<ResponseItem>,
    goal_text: &str,
    environment_details: &str,
    summary: Option<String>,
    focus: Option<String>,
    turn_snapshot: Option<CrossCheckTurnSnapshot>,
    tools: Vec<OpenAiTool>,
) -> Result<(AutoObserverStatus, Option<String>, Option<String>)> {
    let trigger = ObserverTrigger {
        conversation,
        goal_text: goal_text.to_string(),
        environment_details: environment_details.to_string(),
        reason: ObserverReason::CrossCheck {
            forced: true,
            summary,
            focus,
        },
        turn_snapshot,
        tools,
    };
    run_observer_once(runtime, client, trigger)
}

fn read_coordinator_prompt(_config: &Config) -> Option<String> {
    match fs::read_to_string(COORDINATOR_PROMPT_PATH) {
        Ok(text) => {
            let trimmed = text.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            }
        }
        Err(err) => {
            warn!(
                "failed to read coordinator prompt from {}: {err:#}",
                COORDINATOR_PROMPT_PATH
            );
            None
        }
    }
}

fn build_developer_message(
    goal_text: &str,
    environment_details: &str,
    coordinator_prompt: Option<&str>,
) -> (String, String) {
    let prompt_body = coordinator_prompt.unwrap_or("").trim();
    let intro = if prompt_body.is_empty() {
        format!("Environment:
{}", environment_details)
    } else {
        format!("{prompt_body}

Environment:
{environment_details}")
    };
    let primary_goal = format!("**Primary Goal**
{}", goal_text);
    (intro, primary_goal)
}

fn format_environment_details(sandbox: &str) -> String {
    let cwd = std::env::current_dir()
        .map(|dir| dir.display().to_string())
        .unwrap_or_else(|_| "<unknown>".to_string());
    let branch = run_git_command(["rev-parse", "--abbrev-ref", "HEAD"]).unwrap_or_else(|| "<unknown>".to_string());
    let git_status_raw = run_git_command(["status", "--short"]);
    let git_status = match git_status_raw {
        Some(raw) if raw.trim().is_empty() => "  clean".to_string(),
        Some(raw) => raw
            .lines()
            .map(|line| format!("  {line}"))
            .collect::<Vec<_>>()
            .join("\n"),
        None => "  <git status unavailable>".to_string(),
    };
    format!(
        "- Access: {sandbox}\n- Working directory: {cwd}\n- Git branch: {branch}\n- Git status:\n{git_status}"
    )
}

fn run_git_command<const N: usize>(args: [&str; N]) -> Option<String> {
    let output = Command::new("git").args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    String::from_utf8(output.stdout)
        .ok()
        .map(|text| text.trim_end().to_string())
}

#[derive(Clone, Copy)]
struct SchemaFeatures {
    include_agents: bool,
    include_review: bool,
    include_cross_check: bool,
}

impl SchemaFeatures {
    fn from_auto_settings(settings: &AutoDriveSettings) -> Self {
        Self {
            include_agents: settings.agents_enabled,
            include_review: settings.review_enabled,
            include_cross_check: settings.cross_check_enabled,
        }
    }
}

impl Default for SchemaFeatures {
    fn default() -> Self {
        Self {
            include_agents: true,
            include_review: true,
            include_cross_check: true,
        }
    }
}

fn build_schema(active_agents: &[String], features: SchemaFeatures) -> Value {
    let models_enum_values: Vec<Value> = active_agents
        .iter()
        .map(|name| Value::String(name.clone()))
        .collect();

    let models_items_schema = {
        let mut schema = json!({
            "type": "string",
        });
        if !models_enum_values.is_empty() {
            schema["enum"] = Value::Array(models_enum_values.clone());
        }
        schema
    };

    let models_description = {
        let guides = [
            (
                "claude-sonnet-4.5",
                "Default for most coding tasks (along with code-gpt-5-codex) — excels at implementation, tool use, debugging, and testing.",
            ),
            (
                "claude-opus-4.1",
                "Prefer claude-sonnet-4.5 for most tasks, but a good fallback for complex reasoning when other attempts have failed.",
            ),
            (
                "code-gpt-5-codex",
                "Default for most coding tasks (along with claude-sonnet-4.5) - excels at implementation, refactors, multi-file edits and code review.",
            ),
            (
                "code-gpt-5",
                "Use for UI/UX or mixed tasks where explanation, design judgment, or multi-domain reasoning is equally important as code.",
            ),
            (
                "gemini-2.5-pro",
                "Use when you require huge context or multimodal grounding (repo-scale inputs, or search grounding); good for alternative architecture opinions.",
            ),
            (
                "gemini-2.5-flash",
                "Use for fast, high-volume scaffolding, creating minimal repros/tests, or budget-sensitive operations.",
            ),
            (
                "qwen-3-coder",
                "Fast and reasonably effective. Good for providing an alternative opinion when initial attempts fail.",
            ),
        ];

        let mut description = String::from(
            "Preferred agent models for this helper (choose from the valid agent list). Selection guide:",
        );
        let mut any_guides = false;

        for (model, guide) in guides {
            if active_agents.iter().any(|name| name == model) {
                description.push('\n');
                description.push_str("- `");
                description.push_str(model);
                description.push_str("`: ");
                description.push_str(guide);
                any_guides = true;
            }
        }

        if !any_guides {
            description.push_str("\n- No model guides available for the current configuration.");
        }

        description
    };

    let models_request_property = json!({
        "type": "array",
        "description": models_description,
        "items": models_items_schema,
    });

    let mut properties = serde_json::Map::new();
    let mut required: Vec<Value> = Vec::new();

    properties.insert(
        "finish_status".to_string(),
        json!({
            "type": "string",
            "enum": ["continue", "finish_success", "finish_failed"],
            "description": "Prefer 'continue' unless the mission is fully complete or truly blocked. Always consider what further work might be possible to confirm the goal is complete before ending."
        }),
    );
    required.push(Value::String("finish_status".to_string()));

    properties.insert(
        "progress".to_string(),
        json!({
            "type": "object",
            "additionalProperties": false,
            "properties": {
                "past": {
                    "type": ["string", "null"],
                    "minLength": 4,
                    "maxLength": 50,
                    "description": "2-5 words, past-tense, work performed so far."
                },
                "current": {
                    "type": "string",
                    "minLength": 4,
                    "maxLength": 50,
                    "description": "2-5 words, present-tense, what is being worked on now."
                }
            },
            "required": ["past", "current"]
        }),
    );
    required.push(Value::String("progress".to_string()));

    properties.insert(
        "cli".to_string(),
        json!({
            "type": ["object", "null"],
            "additionalProperties": false,
            "description": "The single atomic instruction for the CLI this turn. Set to null only when finish_status != 'continue'.",
            "properties": {
                "context": {
                    "type": ["string", "null"],
                    "maxLength": 1500,
                    "description": "Only info the CLI wouldn’t infer (paths, constraints). Keep it tight."
                },
                "prompt": {
                    "type": "string",
                    "minLength": 4,
                    "maxLength": 240,
                    "description": "Exactly one objective in 1–2 sentences. No step lists."
                }
            },
            "required": ["prompt", "context"]
        }),
    );
    required.push(Value::String("cli".to_string()));

    if features.include_agents {
        properties.insert(
            "agents".to_string(),
            json!({
                "type": ["object", "null"],
                "additionalProperties": false,
                "description": "Parallel help agents for the CLI to spawn. Use as often as possible. Agents are faster, parallelize work and allow exploration of a range of approaches.",
                "properties": {
                    "timing": {
                        "type": "string",
                        "enum": ["parallel", "blocking"],
                        "description": "Parallel: run while the CLI works. Blocking: wait for results before the CLI proceeds."
                    },
                    "list": {
                        "type": "array",
                        "maxItems": 3,
                        "items": {
                            "type": "object",
                            "additionalProperties": false,
                            "properties": {
                                "write": {
                                    "type": "boolean",
                                    "description": "Creates an isolated worktree for each agent and enable writes to that worktree. Default false so that the agent can only read files."
                                },
                                "context": {
                                    "type": ["string", "null"],
                                    "maxLength": 1500,
                                    "description": "Background details (agents can not see the conversation - you must provide any neccessary information here)."
                                },
                                "prompt": {
                                    "type": "string",
                                    "minLength": 8,
                                    "maxLength": 400,
                                    "description": "Outcome-oriented instruction (what to produce)."
                                },
                                "models": models_request_property.clone()
                            },
                            "required": ["prompt", "context", "write", "models"]
                        },
                        "description": "Aim for 1-3 helper agents per turn. More or less is allowed if the situation calls for it."
                    },
                },
                "required": ["timing", "list"]
            }),
        );
        required.push(Value::String("agents".to_string()));
    }

    if features.include_review {
        properties.insert(
            "code_review".to_string(),
            json!({
                "type": ["object", "null"],
                "additionalProperties": false,
                "description": "Starts an optional code review at the start of the turn. Use whenever substantial code has changed. Confirms there are no errors in the implementation using a specialized review model.",
                "properties": {
                    "source": {
                        "type": "string",
                        "enum": ["staged", "commit"],
                        "description": "What to review."
                    },
                    "sha": {
                        "type": ["string", "null"],
                        "minLength": 7,
                        "maxLength": 64,
                        "description": "Required when source='commit'; otherwise null."
                    },
                    "summary": {
                        "type": ["string", "null"],
                        "maxLength": 200,
                        "description": "Optional focus/risks/acceptance criteria."
                    }
                },
                "required": ["source", "sha", "summary"]
            }),
        );
        required.push(Value::String("code_review".to_string()));
    }

    if features.include_cross_check {
        properties.insert(
            "cross_check".to_string(),
            json!({
                "type": ["object", "null"],
                "additionalProperties": false,
                "description": "Optional QA-style cross check to validate the turn outcome. Runs alongside the CLI and prefers failing when anything looks incomplete.",
                "properties": {
                    "summary": {
                        "type": ["string", "null"],
                        "maxLength": 300,
                        "description": "One-line reminder of what the cross check should confirm."
                    },
                    "focus": {
                        "type": ["string", "null"],
                        "maxLength": 400,
                        "description": "Specific risks, flows, or acceptance criteria the cross check must probe."
                    }
                },
                "required": ["summary", "focus"]
            }),
        );
        required.push(Value::String("cross_check".to_string()));
    }

    let mut schema = serde_json::Map::new();
    schema.insert(
        "$schema".to_string(),
        Value::String("https://json-schema.org/draft/2020-12/schema".to_string()),
    );
    schema.insert(
        "title".to_string(),
        Value::String("Coordinator Turn (CLI-first; agents + review background)".to_string()),
    );
    schema.insert("type".to_string(), Value::String("object".to_string()));
    schema.insert("additionalProperties".to_string(), Value::Bool(false));
    schema.insert("properties".to_string(), Value::Object(properties));
    schema.insert("required".to_string(), Value::Array(required));

    Value::Object(schema)
}


struct RequestStreamResult {
    output_text: String,
    response_items: Vec<ResponseItem>,
}

fn request_coordinator_decision(
    runtime: &tokio::runtime::Runtime,
    client: &ModelClient,
    developer_intro: &str,
    primary_goal: &str,
    schema: &Value,
    conversation: Vec<ResponseItem>,
    auto_instructions: Option<&str>,
    app_event_tx: &AppEventSender,
    cancel_token: &CancellationToken,
) -> Result<ParsedCoordinatorDecision> {
    let (raw, response_items) = request_decision(
        runtime,
        client,
        developer_intro,
        primary_goal,
        schema,
        &conversation,
        auto_instructions,
        app_event_tx,
        cancel_token,
    )?;
    let (mut decision, value) = parse_decision(&raw)?;
    debug!("[Auto coordinator] model decision: {:?}", value);
    decision.response_items = response_items;
    Ok(decision)
}

fn request_decision(
    runtime: &tokio::runtime::Runtime,
    client: &ModelClient,
    developer_intro: &str,
    primary_goal: &str,
    schema: &Value,
    conversation: &[ResponseItem],
    auto_instructions: Option<&str>,
    app_event_tx: &AppEventSender,
    cancel_token: &CancellationToken,
) -> Result<(String, Vec<ResponseItem>)> {
    match request_decision_with_model(
        runtime,
        client,
        developer_intro,
        primary_goal,
        schema,
        conversation,
        auto_instructions,
        app_event_tx,
        cancel_token,
        MODEL_SLUG,
    ) {
        Ok(result) => Ok((result.output_text, result.response_items)),
        Err(err) => {
            let fallback_slug = client.default_model_slug().to_string();
            if fallback_slug != MODEL_SLUG && should_retry_with_default_model(&err) {
                debug!(
                    preferred = %MODEL_SLUG,
                    fallback = %fallback_slug,
                    "auto coordinator falling back to configured model after invalid model error"
                );
                let original_error = err.to_string();
                return request_decision_with_model(
                    runtime,
                    client,
                    developer_intro,
                    primary_goal,
                    schema,
                    conversation,
                    auto_instructions,
                    app_event_tx,
                    cancel_token,
                    &fallback_slug,
                )
                .map(|res| (res.output_text, res.response_items))
                .map_err(|fallback_err| {
                    fallback_err.context(format!(
                        "coordinator fallback with model '{}' failed after original error: {}",
                        fallback_slug, original_error
                    ))
                });
            }
            Err(err)
        }
    }
}

fn request_decision_with_model(
    runtime: &tokio::runtime::Runtime,
    client: &ModelClient,
    developer_intro: &str,
    primary_goal: &str,
    schema: &Value,
    conversation: &[ResponseItem],
    auto_instructions: Option<&str>,
    app_event_tx: &AppEventSender,
    cancel_token: &CancellationToken,
    model_slug: &str,
) -> Result<RequestStreamResult> {
    let developer_intro = developer_intro.to_string();
    let primary_goal = primary_goal.to_string();
    let schema = schema.clone();
    let conversation: Vec<ResponseItem> = conversation.to_vec();
    let auto_instructions = auto_instructions.map(|text| text.to_string());
    let tx = app_event_tx.clone();
    let cancel = cancel_token.clone();
    let classify = |error: &anyhow::Error| classify_model_error(error);
    let options = RetryOptions::with_defaults(MAX_RETRY_ELAPSED);

    let result = runtime.block_on(async move {
        retry_with_backoff(
            || {
                let instructions = auto_instructions.clone();
                let prompt = build_prompt_for_model(
                    &developer_intro,
                    &primary_goal,
                    &schema,
                    &conversation,
                    model_slug,
                    instructions.as_deref(),
                );
                let tx_inner = tx.clone();
                async move {
                    #[cfg(feature = "dev-faults")]
                    if let Some(fault) = next_fault(FaultScope::AutoDrive) {
                        let err = fault_to_error(fault);
                        return Err(err);
                    }
                    let mut stream = client.stream(&prompt).await?;
                    let mut out = String::new();
                    let mut response_items: Vec<ResponseItem> = Vec::new();
                    let mut reasoning_delta_accumulator = String::new();
                    while let Some(ev) = stream.next().await {
                        match ev {
                            Ok(ResponseEvent::OutputTextDelta { delta, .. }) => {
                                out.push_str(&delta);
                            }
                            Ok(ResponseEvent::OutputItemDone { item, .. }) => {
                                if let ResponseItem::Message { content, .. } = &item {
                                    for c in content {
                                        if let ContentItem::OutputText { text } = c {
                                            out.push_str(text);
                                        }
                                    }
                                }
                                if matches!(item, ResponseItem::Reasoning { .. }) {
                                    reasoning_delta_accumulator.clear();
                                }
                                response_items.push(item);
                            }
                            Ok(ResponseEvent::ReasoningSummaryDelta {
                                delta,
                                summary_index,
                                ..
                            }) => {
                                let cleaned = strip_role_prefix(&delta);
                                reasoning_delta_accumulator.push_str(cleaned);
                                let message = cleaned.to_string();
                                tx_inner.send(AppEvent::AutoCoordinatorThinking {
                                    delta: message,
                                    summary_index,
                                });
                            }
                            Ok(ResponseEvent::ReasoningContentDelta { delta, .. }) => {
                                let cleaned = strip_role_prefix(&delta);
                                reasoning_delta_accumulator.push_str(cleaned);
                                let message = cleaned.to_string();
                                tx_inner.send(AppEvent::AutoCoordinatorThinking {
                                    delta: message,
                                    summary_index: None,
                                });
                            }
                            Ok(ResponseEvent::Completed { .. }) => break,
                            Err(err) => return Err(err.into()),
                            _ => {}
                        }
                    }
                    if !reasoning_delta_accumulator.trim().is_empty()
                        && !response_items
                            .iter()
                            .any(|item| matches!(item, ResponseItem::Reasoning { .. }))
                    {
                        response_items.push(ResponseItem::Reasoning {
                            id: String::new(),
                            summary: Vec::new(),
                            content: Some(vec![ReasoningItemContent::ReasoningText {
                                text: reasoning_delta_accumulator.trim().to_string(),
                            }]),
                            encrypted_content: None,
                        });
                    }
                    Ok(RequestStreamResult {
                        output_text: out,
                        response_items,
                    })
                }
            },
            classify,
            options,
            &cancel,
            |status| {
                let human_delay = status
                    .sleep
                    .map(format_duration)
                    .unwrap_or_else(|| "0s".to_string());
                let elapsed = format_duration(status.elapsed);
                let prefix = if status.is_rate_limit {
                    "Rate limit"
                } else {
                    "Transient error"
                };
                let attempt = status.attempt;
                let resume_str = status.resume_at.and_then(|resume| {
                    let now = Instant::now();
                    if resume <= now {
                        Some("now".to_string())
                    } else {
                        let remaining = resume.duration_since(now);
                        SystemTime::now()
                            .checked_add(remaining)
                            .map(|time| {
                                let local: DateTime<Local> = time.into();
                                local.format("%Y-%m-%d %H:%M:%S").to_string()
                            })
                    }
                });
                let message = format!(
                    "{prefix} (attempt {attempt}): {}; retrying in {human_delay} (elapsed {elapsed}){}",
                    status.reason,
                    resume_str
                        .map(|s| format!("; next attempt at {s}"))
                        .unwrap_or_default()
                );
                let _ = tx.send(AppEvent::AutoCoordinatorThinking {
                    delta: message,
                    summary_index: None,
                });
            },
        )
        .await
    });

    match result {
        Ok(output) => Ok(output),
        Err(RetryError::Aborted) => Err(anyhow!(AutoCoordinatorCancelled)),
        Err(RetryError::Fatal(err)) => Err(err),
        Err(RetryError::Timeout { elapsed, last_error }) => Err(last_error.context(format!(
            "auto coordinator retry window exceeded after {}",
            format_duration(elapsed)
        ))),
    }
}

fn build_prompt_for_model(
    developer_intro: &str,
    primary_goal: &str,
    schema: &Value,
    conversation: &[ResponseItem],
    model_slug: &str,
    auto_instructions: Option<&str>,
) -> Prompt {
    let mut prompt = Prompt::default();
    prompt.store = true;
    if let Some(instructions) = auto_instructions {
        let trimmed = instructions.trim();
        if !trimmed.is_empty() {
            prompt
                .input
                .push(make_message("developer", trimmed.to_string()));
        }
    }
    prompt
        .input
        .push(make_message("developer", developer_intro.to_string()));
    prompt
        .input
        .push(make_message("developer", primary_goal.to_string()));
    prompt.input.extend(conversation.iter().cloned());
    prompt.text_format = Some(TextFormat {
        r#type: "json_schema".to_string(),
        name: Some(SCHEMA_NAME.to_string()),
        strict: Some(true),
        schema: Some(schema.clone()),
    });
    prompt.model_override = Some(model_slug.to_string());
    let family = find_family_for_model(model_slug)
        .unwrap_or_else(|| derive_default_model_family(model_slug));
    prompt.model_family_override = Some(family);
    prompt
}

fn should_retry_with_default_model(err: &anyhow::Error) -> bool {
    err.chain().any(|cause| {
        if let Some(code_err) = cause.downcast_ref::<CodexErr>() {
            if let CodexErr::UnexpectedStatus(err) = code_err {
                if !err.status.is_client_error() {
                    return false;
                }
                let body_lower = err.body.to_lowercase();
                return body_lower.contains("invalid model")
                    || body_lower.contains("unknown model")
                    || body_lower.contains("model_not_found")
                    || body_lower.contains("model does not exist");
            }
        }
        false
    })
}

pub(crate) fn classify_model_error(error: &anyhow::Error) -> RetryDecision {
    if let Some(code_err) = find_in_chain::<CodexErr>(error) {
        match code_err {
            CodexErr::Stream(message, _) => {
                return RetryDecision::RetryAfterBackoff {
                    reason: format!("model stream error: {message}"),
                };
            }
            CodexErr::Timeout => {
                return RetryDecision::RetryAfterBackoff {
                    reason: "model request timed out".to_string(),
                };
            }
            CodexErr::UnexpectedStatus(err) => {
                let status = err.status;
                let body = &err.body;
                if status == StatusCode::REQUEST_TIMEOUT || status.as_u16() == 408 {
                    return RetryDecision::RetryAfterBackoff {
                        reason: format!("provider returned {status}"),
                    };
                }
                if status.as_u16() == 499 {
                    return RetryDecision::RetryAfterBackoff {
                        reason: "client closed request (499)".to_string(),
                    };
                }
                if status == StatusCode::TOO_MANY_REQUESTS {
                    if let Some(wait_until) = parse_rate_limit_hint(body) {
                        return RetryDecision::RateLimited {
                            wait_until,
                            reason: "rate limited; waiting for reset".to_string(),
                        };
                    }
                    return RetryDecision::RetryAfterBackoff {
                        reason: "rate limited (429)".to_string(),
                    };
                }
                if status.is_client_error() {
                    return RetryDecision::Fatal(anyhow!(error.to_string()));
                }
                if status.is_server_error() {
                    return RetryDecision::RetryAfterBackoff {
                        reason: format!("server error {status}"),
                    };
                }
            }
            CodexErr::UsageLimitReached(limit) => {
                if let Some(seconds) = limit.resets_in_seconds {
                    let wait_until = compute_rate_limit_wait(Duration::from_secs(seconds));
                    return RetryDecision::RateLimited {
                        wait_until,
                        reason: "usage limit reached".to_string(),
                    };
                }
                return RetryDecision::RetryAfterBackoff {
                    reason: "usage limit reached".to_string(),
                };
            }
            CodexErr::UsageNotIncluded => {
                return RetryDecision::Fatal(anyhow!(error.to_string()));
            }
            CodexErr::ServerError(_) => {
                return RetryDecision::RetryAfterBackoff {
                    reason: error.to_string(),
                };
            }
            CodexErr::RetryLimit(status) => {
                return RetryDecision::Fatal(anyhow!("retry limit exceeded (status {status})"));
            }
            CodexErr::Reqwest(req_err) => {
                return classify_reqwest_error(req_err);
            }
            CodexErr::Io(io_err) => {
                if io_err.kind() == std::io::ErrorKind::TimedOut {
                    return RetryDecision::RetryAfterBackoff {
                        reason: "network timeout".to_string(),
                    };
                }
            }
            _ => {}
        }
    }

    if let Some(req_err) = find_in_chain::<reqwest::Error>(error) {
        return classify_reqwest_error(req_err);
    }

    if let Some(io_err) = find_in_chain::<std::io::Error>(error) {
        if io_err.kind() == std::io::ErrorKind::TimedOut {
            return RetryDecision::RetryAfterBackoff {
                reason: "network timeout".to_string(),
            };
        }
    }

    RetryDecision::Fatal(anyhow!(error.to_string()))
}

fn classify_reqwest_error(err: &reqwest::Error) -> RetryDecision {
    if err.is_timeout() || err.is_connect() || err.is_request() && err.status().is_none() {
        return RetryDecision::RetryAfterBackoff {
            reason: format!("network error: {err}"),
        };
    }

    if let Some(status) = err.status() {
        if status == StatusCode::TOO_MANY_REQUESTS {
            return RetryDecision::RetryAfterBackoff {
                reason: "rate limited (429)".to_string(),
            };
        }
        if status == StatusCode::REQUEST_TIMEOUT || status.as_u16() == 408 {
            return RetryDecision::RetryAfterBackoff {
                reason: format!("provider returned {status}"),
            };
        }
        if status.as_u16() == 499 {
            return RetryDecision::RetryAfterBackoff {
                reason: "client closed request (499)".to_string(),
            };
        }
        if status.is_server_error() {
            return RetryDecision::RetryAfterBackoff {
                reason: format!("server error {status}"),
            };
        }
        if status.is_client_error() {
            return RetryDecision::Fatal(anyhow!(err.to_string()));
        }
    }

    RetryDecision::Fatal(anyhow!(err.to_string()))
}

fn parse_rate_limit_hint(body: &str) -> Option<Instant> {
    let value: serde_json::Value = serde_json::from_str(body).ok()?;
    let error_obj = value.get("error").unwrap_or(&value);

    if let Some(seconds) = extract_seconds(error_obj) {
        return Some(compute_rate_limit_wait(seconds));
    }

    if let Some(reset_at) = extract_reset_at(error_obj) {
        return Some(reset_at);
    }

    None
}

fn extract_seconds(value: &serde_json::Value) -> Option<Duration> {
    let fields = [
        "reset_seconds",
        "reset_in_seconds",
        "resets_in_seconds",
        "x-ratelimit-reset",
        "x-ratelimit-reset-requests",
    ];
    for key in fields {
        if let Some(seconds) = value.get(key) {
            if let Some(num) = seconds.as_f64() {
                if num.is_sign_negative() {
                    continue;
                }
                return Some(Duration::from_secs_f64(num));
            }
            if let Some(text) = seconds.as_str() {
                if let Ok(num) = text.parse::<f64>() {
                    if num.is_sign_negative() {
                        continue;
                    }
                    return Some(Duration::from_secs_f64(num));
                }
            }
        }
    }
    None
}

fn extract_reset_at(value: &serde_json::Value) -> Option<Instant> {
    let reset_at = value.get("reset_at").and_then(|v| v.as_str())?;
    let parsed = DateTime::parse_from_rfc3339(reset_at)
        .or_else(|_| DateTime::parse_from_str(reset_at, "%+"))
        .ok()?;
    let reset_utc = parsed.with_timezone(&Utc);
    let now = Utc::now();
    let duration = reset_utc.signed_duration_since(now).to_std().unwrap_or_default();
    Some(compute_rate_limit_wait(duration))
}

fn compute_rate_limit_wait(base: Duration) -> Instant {
    let mut wait = if base > Duration::ZERO { base } else { Duration::ZERO };
    wait += RATE_LIMIT_BUFFER;
    wait += random_jitter(RATE_LIMIT_JITTER_MAX);
    Instant::now() + wait
}

fn random_jitter(max: Duration) -> Duration {
    if max.is_zero() {
        return Duration::ZERO;
    }
    let mut rng = rand::rng();
    let jitter = rng.random_range(0.0..max.as_secs_f64());
    Duration::from_secs_f64(jitter)
}

fn find_in_chain<'a, T: std::error::Error + 'static>(error: &'a anyhow::Error) -> Option<&'a T> {
    for cause in error.chain() {
        if let Some(specific) = cause.downcast_ref::<T>() {
            return Some(specific);
        }
    }
    None
}


struct RecoverableDecisionError {
    summary: String,
    guidance: Option<String>,
}

fn classify_recoverable_decision_error(err: &anyhow::Error) -> Option<RecoverableDecisionError> {
    let text = err.to_string();
    let lower = text.to_ascii_lowercase();

    if lower.contains("missing cli prompt for continue") {
        return Some(RecoverableDecisionError {
            summary: "missing CLI prompt for `finish_status: \"continue\"`".to_string(),
            guidance: Some(
                "Include a non-empty `cli.prompt` (and optional context) whenever `finish_status` is `\"continue\"`."
                    .to_string(),
            ),
        });
    }

    if lower.contains("legacy model response missing cli_prompt for continue") {
        return Some(RecoverableDecisionError {
            summary: "legacy response omitted `cli_prompt` for continue turn".to_string(),
            guidance: Some(
                "Legacy coordinator responses must populate `cli_prompt` when the turn continues."
                    .to_string(),
            ),
        });
    }

    if lower.contains(" is empty") {
        if let Some((field, _)) = text.split_once(" is empty") {
            let field_trimmed = field.trim().trim_matches('`');
            if !field_trimmed.is_empty() {
                let summary = format!("`{field_trimmed}` was empty");
                let guidance = format!(
                    "Provide a meaningful value for `{field_trimmed}` instead of leaving it blank."
                );
                return Some(RecoverableDecisionError {
                    summary,
                    guidance: Some(guidance),
                });
            }
        }
    }

    if lower.contains("unexpected finish_status") {
        let extracted = text
            .split('\'')
            .nth(1)
            .filter(|value| !value.is_empty())
            .map(|value| format!("unexpected finish_status '{value}'"))
            .unwrap_or_else(|| "unexpected finish_status".to_string());
        return Some(RecoverableDecisionError {
            summary: extracted,
            guidance: Some(
                "Use `finish_status` values: `continue`, `finish_success`, or `finish_failed`."
                    .to_string(),
            ),
        });
    }

    if lower.contains("model response was not valid json") || lower.contains("parsing json from model output") {
        return Some(RecoverableDecisionError {
            summary: "response was not valid JSON".to_string(),
            guidance: Some(
                "Return strictly valid JSON that matches the `auto_coordinator_flow` schema without extra prose."
                    .to_string(),
            ),
        });
    }

    if lower.contains("decoding coordinator decision failed") {
        return Some(RecoverableDecisionError {
            summary: "response did not match the coordinator schema".to_string(),
            guidance: Some(
                "Ensure every required field is present and spelled correctly per the coordinator schema."
                    .to_string(),
            ),
        });
    }

    if lower.contains("invalid code_review source") || lower.contains("invalid review source") {
        return Some(RecoverableDecisionError {
            summary: "invalid code_review source".to_string(),
            guidance: Some(
                "Set `code_review.source` to `\"staged\"` or `\"commit\"` only, matching the documented schema."
                    .to_string(),
            ),
        });
    }

    if lower.contains("code_review requires sha when source='commit'")
        || lower.contains("review requires sha when source='commit'")
    {
        return Some(RecoverableDecisionError {
            summary: "commit code_review missing sha".to_string(),
            guidance: Some(
                "When `code_review.source` is `\"commit\"`, include a non-empty commit SHA in `code_review.sha`."
                    .to_string(),
            ),
        });
    }

    None
}

fn push_unique_guidance(guidance: &mut Vec<String>, message: &str) {
    let trimmed = message.trim();
    if trimmed.is_empty() {
        return;
    }
    if guidance.iter().any(|existing| existing == trimmed) {
        return;
    }
    guidance.push(trimmed.to_string());
}


fn parse_decision(raw: &str) -> Result<(ParsedCoordinatorDecision, Value)> {
    let value: Value = match serde_json::from_str(raw) {
        Ok(v) => v,
        Err(_) => {
            let Some(json_blob) = extract_first_json_object(raw) else {
                return Err(anyhow!("model response was not valid JSON"));
            };
            serde_json::from_str(&json_blob).context("parsing JSON from model output")?
        }
    };
    match serde_json::from_value::<CoordinatorDecisionNew>(value.clone()) {
        Ok(decision) => {
            let status = parse_finish_status(&decision.finish_status)?;
            let parsed = convert_decision_new(decision, status)?;
            Ok((parsed, value))
        }
        Err(new_err) => {
            let decision: CoordinatorDecisionLegacy = serde_json::from_value(value.clone()).map_err(|legacy_err| {
                let payload = serde_json::to_string(&value).unwrap_or_else(|_| "<unprintable json>".to_string());
                let snippet = if payload.len() > 2000 {
                    format!("{}…", &payload[..2000])
                } else {
                    payload
                };
                anyhow!("decoding coordinator decision failed: new_schema_err={new_err}; legacy_err={legacy_err}; payload_snippet={snippet}")
            })?;
            let status = parse_finish_status(&decision.finish_status)?;
            let parsed = convert_decision_legacy(decision, status)?;
            Ok((parsed, value))
        }
    }
}

fn parse_finish_status(finish_status: &str) -> Result<AutoCoordinatorStatus> {
    let normalized = finish_status.trim().to_ascii_lowercase();
    match normalized.as_str() {
        "continue" => Ok(AutoCoordinatorStatus::Continue),
        "finish_success" => Ok(AutoCoordinatorStatus::Success),
        "finish_failed" => Ok(AutoCoordinatorStatus::Failed),
        other => Err(anyhow!("unexpected finish_status '{other}'")),
    }
}

fn convert_decision_new(
    decision: CoordinatorDecisionNew,
    status: AutoCoordinatorStatus,
) -> Result<ParsedCoordinatorDecision> {
    let CoordinatorDecisionNew {
        finish_status: _,
        progress,
        cli,
        agents: agent_payloads,
        code_review,
        cross_check,
    } = decision;

    let progress_past = clean_optional(progress.past);
    let progress_current = clean_optional(progress.current);

    let cli = match (status, cli) {
        (AutoCoordinatorStatus::Continue, Some(payload)) => Some(CliAction {
            prompt: clean_required(&payload.prompt, "cli.prompt")?,
            context: clean_optional(payload.context),
        }),
        (AutoCoordinatorStatus::Continue, None) => {
            return Err(anyhow!("model response missing cli prompt for continue"));
        }
        (_, Some(_payload)) => None,
        (_, None) => None,
    };

    let mut agent_actions: Vec<AgentAction> = Vec::new();
    let mut agents_timing: Option<AutoTurnAgentsTiming> = None;
    if let Some(payloads) = agent_payloads {
        match payloads {
            AgentsField::List(list) => {
                for payload in list {
                    let AgentPayload {
                        prompt,
                        context,
                        write,
                        models,
                    } = payload;
                    let prompt = clean_required(&prompt, "agents[*].prompt")?;
                    agent_actions.push(AgentAction {
                        prompt,
                        context: clean_optional(context),
                        write,
                        models: clean_models(models),
                    });
                }
            }
            AgentsField::Object(plan) => {
                let AgentsPayload {
                    timing,
                    models,
                    requests,
                } = plan;
                if let Some(timing_value) = timing {
                    agents_timing = Some(timing_value.into());
                }
                let batch_models = clean_models(models);
                for payload in requests {
                    let AgentPayload {
                        prompt,
                        context,
                        write,
                        models,
                    } = payload;
                    let prompt = clean_required(&prompt, "agents.requests[*].prompt")?;
                    let models = clean_models(models).or_else(|| batch_models.clone());
                    agent_actions.push(AgentAction {
                        prompt,
                        context: clean_optional(context),
                        write,
                        models,
                    });
                }
            }
        }
    }

    let code_review = match code_review {
        Some(payload) => Some(CodeReviewAction {
            commit: build_review_commit(payload.source, payload.sha, payload.summary)?,
        }),
        None => None,
    };

    let cross_check = match cross_check {
        Some(payload) => Some(CrossCheckAction {
            summary: clean_optional(payload.summary),
            focus: clean_optional(payload.focus),
            forced: false,
        }),
        None => None,
    };

    Ok(ParsedCoordinatorDecision {
        status,
        progress_past,
        progress_current,
        cli,
        agents_timing,
        agents: agent_actions,
        code_review,
        cross_check,
        response_items: Vec::new(),
    })
}

fn convert_decision_legacy(
    decision: CoordinatorDecisionLegacy,
    status: AutoCoordinatorStatus,
) -> Result<ParsedCoordinatorDecision> {
    let CoordinatorDecisionLegacy {
        finish_status: _,
        progress_past,
        progress_current,
        cli_context,
        cli_prompt,
        review_commit,
    } = decision;

    let progress_past = clean_optional(progress_past);
    let progress_current = clean_optional(progress_current);
    let context = clean_optional(cli_context);

    let cli = match (status, cli_prompt) {
        (AutoCoordinatorStatus::Continue, Some(prompt)) => Some(CliAction {
            prompt: clean_required(&prompt, "cli_prompt")?,
            context: context.clone(),
        }),
        (AutoCoordinatorStatus::Continue, None) => {
            return Err(anyhow!("legacy model response missing cli_prompt for continue"));
        }
        (_, Some(prompt)) => Some(CliAction {
            prompt: clean_required(&prompt, "cli_prompt")?,
            context: context.clone(),
        }),
        (_, None) => None,
    };

    let code_review = match review_commit {
        Some(payload) => Some(CodeReviewAction {
            commit: payload.try_into_commit()?,
        }),
        None => None,
    };

    Ok(ParsedCoordinatorDecision {
        status,
        progress_past,
        progress_current,
        cli,
        agents_timing: None,
        agents: Vec::new(),
        code_review,
        cross_check: None,
        response_items: Vec::new(),
    })
}

fn clean_optional(input: Option<String>) -> Option<String> {
    input.and_then(|value| {
        let trimmed = value.trim();
        if trimmed.is_empty() {
            None
        } else {
            let without_prefix = strip_role_prefix(trimmed);
            let final_trimmed = without_prefix.trim();
            if final_trimmed.is_empty() {
                None
            } else {
                Some(final_trimmed.to_string())
            }
        }
    })
}

fn clean_models(models: Option<Vec<String>>) -> Option<Vec<String>> {
    let mut cleaned: Vec<String> = models?
        .into_iter()
        .filter_map(|model| {
            let trimmed = model.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            }
        })
        .collect();

    if cleaned.is_empty() {
        return None;
    }

    cleaned.sort();
    cleaned.dedup();
    Some(cleaned)
}

fn clean_required(value: &str, field: &str) -> Result<String> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        Err(anyhow!("{field} is empty"))
    } else {
        let without_prefix = strip_role_prefix(trimmed);
        let final_trimmed = without_prefix.trim();
        if final_trimmed.is_empty() {
            Err(anyhow!("{field} is empty"))
        } else {
            Ok(final_trimmed.to_string())
        }
    }
}

fn build_review_commit(
    source: String,
    sha: Option<String>,
    summary: Option<String>,
) -> Result<AutoReviewCommit> {
    let source_enum = match source.trim().to_ascii_lowercase().as_str() {
        "staged" => AutoReviewCommitSource::Staged,
        "commit" => AutoReviewCommitSource::Commit,
        other => return Err(anyhow!("invalid code_review source '{other}'")),
    };

    let sha_clean = clean_optional(sha);
    if matches!(source_enum, AutoReviewCommitSource::Commit) && sha_clean.is_none() {
        return Err(anyhow!("code_review requires sha when source='commit'"));
    }

    Ok(AutoReviewCommit {
        source: source_enum,
        sha: sha_clean,
        summary: clean_optional(summary),
    })
}

impl ReviewCommitPayloadLegacy {
    fn try_into_commit(self) -> Result<AutoReviewCommit> {
        build_review_commit(self.source, self.sha, self.summary)
    }
}

fn cli_action_to_event(action: &CliAction) -> AutoTurnCliAction {
    AutoTurnCliAction {
        prompt: action.prompt.clone(),
        context: action.context.clone(),
    }
}

fn agent_action_to_event(action: &AgentAction) -> AutoTurnAgentsAction {
    AutoTurnAgentsAction {
        prompt: action.prompt.clone(),
        context: action.context.clone(),
        write: action.write,
        models: action.models.clone(),
    }
}

fn code_review_action_to_event(action: &CodeReviewAction) -> AutoTurnCodeReviewAction {
    AutoTurnCodeReviewAction {
        commit: action.commit.clone(),
    }
}

fn cross_check_action_to_event(action: &CrossCheckAction) -> AutoTurnCrossCheckAction {
    AutoTurnCrossCheckAction {
        summary: action.summary.clone(),
        focus: action.focus.clone(),
        forced: action.forced,
    }
}

pub(super) fn extract_first_json_object(input: &str) -> Option<String> {
    let mut depth = 0usize;
    let mut in_str = false;
    let mut escape = false;
    let mut start: Option<usize> = None;
    for (idx, ch) in input.char_indices() {
        if in_str {
            if escape {
                escape = false;
                continue;
            }
            match ch {
                '"' => in_str = false,
                '\\' => escape = true,
                _ => {}
            }
            continue;
        }
        match ch {
            '"' => in_str = true,
            '{' => {
                if depth == 0 {
                    start = Some(idx);
                }
                depth += 1;
            }
            '}' => {
                if depth == 0 {
                    continue;
                }
                depth -= 1;
                if depth == 0 {
                    let Some(s) = start else { return None; };
                    return Some(input[s..=idx].to_string());
                }
            }
            _ => {}
        }
    }
    None
}

pub(super) fn make_message(role: &str, text: String) -> ResponseItem {
    let content = if role.eq_ignore_ascii_case("assistant") {
        ContentItem::OutputText { text }
    } else {
        ContentItem::InputText { text }
    };

    ResponseItem::Message {
        id: None,
        role: role.to_string(),
        content: vec![content],
    }
}

fn strip_role_prefix(input: &str) -> &str {
    const PREFIXES: [&str; 2] = ["Coordinator:", "CLI:"];
    for prefix in PREFIXES {
        if let Some(head) = input.get(..prefix.len()) {
            if head.eq_ignore_ascii_case(prefix) {
                let rest = input
                    .get(prefix.len()..)
                    .unwrap_or_default();
                return rest.strip_prefix(' ').unwrap_or(rest);
            }
        }
    }
    input
}

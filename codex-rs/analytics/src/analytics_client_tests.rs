use crate::client::AnalyticsEventsQueue;
use crate::events::AppServerRpcTransport;
use crate::events::CodexAppMentionedEventRequest;
use crate::events::CodexAppServerClientMetadata;
use crate::events::CodexAppUsedEventRequest;
use crate::events::CodexPluginEventRequest;
use crate::events::CodexPluginUsedEventRequest;
use crate::events::CodexRuntimeMetadata;
use crate::events::ThreadInitializationMode;
use crate::events::ThreadInitializedEvent;
use crate::events::ThreadInitializedEventParams;
use crate::events::TrackEventRequest;
use crate::events::codex_app_metadata;
use crate::events::codex_plugin_metadata;
use crate::events::codex_plugin_used_metadata;
use crate::facts::AnalyticsFact;
use crate::facts::AppInvocation;
use crate::facts::AppMentionedInput;
use crate::facts::AppUsedInput;
use crate::facts::CustomAnalyticsFact;
use crate::facts::InvocationType;
use crate::facts::PluginState;
use crate::facts::PluginStateChangedInput;
use crate::facts::PluginUsedInput;
use crate::facts::SkillInvocation;
use crate::facts::SkillInvokedInput;
use crate::facts::TrackEventsContext;
use crate::reducer::AnalyticsReducer;
use crate::reducer::normalize_path_for_skill_id;
use crate::reducer::skill_id_for_local_skill;
use codex_app_server_protocol::ApprovalsReviewer as AppServerApprovalsReviewer;
use codex_app_server_protocol::AskForApproval as AppServerAskForApproval;
use codex_app_server_protocol::ClientInfo;
use codex_app_server_protocol::ClientResponse;
use codex_app_server_protocol::InitializeCapabilities;
use codex_app_server_protocol::InitializeParams;
use codex_app_server_protocol::RequestId;
use codex_app_server_protocol::SandboxPolicy as AppServerSandboxPolicy;
use codex_app_server_protocol::SessionSource as AppServerSessionSource;
use codex_app_server_protocol::Thread;
use codex_app_server_protocol::ThreadResumeResponse;
use codex_app_server_protocol::ThreadStartResponse;
use codex_app_server_protocol::ThreadStatus as AppServerThreadStatus;
use codex_login::default_client::DEFAULT_ORIGINATOR;
use codex_login::default_client::originator;
use codex_plugin::AppConnectorId;
use codex_plugin::PluginCapabilitySummary;
use codex_plugin::PluginId;
use codex_plugin::PluginTelemetryMetadata;
use codex_protocol::approvals::NetworkApprovalProtocol;
use codex_protocol::config_types::ApprovalsReviewer;
use codex_protocol::config_types::ModeKind;
use codex_protocol::models::NetworkPermissions as CoreNetworkPermissions;
use codex_protocol::models::PermissionProfile as CorePermissionProfile;
use codex_protocol::protocol::AskForApproval;
use codex_protocol::protocol::HookEventName;
use codex_protocol::protocol::HookRunStatus;
use codex_protocol::protocol::HookSource;
use codex_protocol::protocol::SessionSource;
use codex_protocol::protocol::SubAgentSource;
use codex_protocol::protocol::ThreadSource;
use codex_protocol::protocol::TokenUsage;
use codex_protocol::request_permissions::PermissionGrantScope as CorePermissionGrantScope;
use codex_protocol::request_permissions::RequestPermissionProfile as CoreRequestPermissionProfile;
use codex_protocol::request_permissions::RequestPermissionsResponse as CoreRequestPermissionsResponse;
use codex_utils_absolute_path::test_support::PathBufExt;
use codex_utils_absolute_path::test_support::test_path_buf;
use pretty_assertions::assert_eq;
use serde_json::json;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use tokio::sync::mpsc;

fn sample_thread(thread_id: &str, ephemeral: bool) -> Thread {
    Thread {
        id: thread_id.to_string(),
        preview: "first prompt".to_string(),
        ephemeral,
        model_provider: "openai".to_string(),
        created_at: 1,
        updated_at: 2,
        status: AppServerThreadStatus::Idle,
        path: None,
        cwd: PathBuf::from("/tmp"),
        cli_version: "0.0.0".to_string(),
        source: AppServerSessionSource::Exec,
        agent_nickname: None,
        agent_role: None,
        git_info: None,
        name: None,
        turns: Vec::new(),
    }
}

fn sample_thread_start_response(thread_id: &str, ephemeral: bool, model: &str) -> ClientResponse {
    ClientResponse::ThreadStart {
        request_id: RequestId::Integer(1),
        response: ThreadStartResponse {
            thread: sample_thread(thread_id, ephemeral),
            model: model.to_string(),
            model_provider: "openai".to_string(),
            service_tier: None,
            cwd: PathBuf::from("/tmp"),
            approval_policy: AppServerAskForApproval::OnFailure,
            approvals_reviewer: AppServerApprovalsReviewer::User,
            sandbox: AppServerSandboxPolicy::DangerFullAccess,
            reasoning_effort: None,
        },
    }
}

fn sample_thread_resume_response(thread_id: &str, ephemeral: bool, model: &str) -> ClientResponse {
    ClientResponse::ThreadResume {
        request_id: RequestId::Integer(2),
        response: ThreadResumeResponse {
            thread: sample_thread(thread_id, ephemeral),
            model: model.to_string(),
            model_provider: "openai".to_string(),
            service_tier: None,
            cwd: PathBuf::from("/tmp"),
            approval_policy: AppServerAskForApproval::OnFailure,
            approvals_reviewer: AppServerApprovalsReviewer::User,
            sandbox: AppServerSandboxPolicy::DangerFullAccess,
            reasoning_effort: None,
        },
    }
}

fn expected_absolute_path(path: &PathBuf) -> String {
    std::fs::canonicalize(path)
        .unwrap_or_else(|_| path.to_path_buf())
        .to_string_lossy()
        .replace('\\', "/")
}

#[test]
fn normalize_path_for_skill_id_repo_scoped_uses_relative_path() {
    let repo_root = PathBuf::from("/repo/root");
    let skill_path = PathBuf::from("/repo/root/.codex/skills/doc/SKILL.md");

    let path = normalize_path_for_skill_id(
        Some("https://example.com/repo.git"),
        Some(repo_root.as_path()),
        skill_path.as_path(),
    );

    assert_eq!(path, ".codex/skills/doc/SKILL.md");
}

#[test]
fn normalize_path_for_skill_id_user_scoped_uses_absolute_path() {
    let skill_path = PathBuf::from("/Users/abc/.codex/skills/doc/SKILL.md");

    let path = normalize_path_for_skill_id(
        /*repo_url*/ None,
        /*repo_root*/ None,
        skill_path.as_path(),
    );
    let expected = expected_absolute_path(&skill_path);

    assert_eq!(path, expected);
}

#[test]
fn normalize_path_for_skill_id_admin_scoped_uses_absolute_path() {
    let skill_path = PathBuf::from("/etc/codex/skills/doc/SKILL.md");

    let path = normalize_path_for_skill_id(
        /*repo_url*/ None,
        /*repo_root*/ None,
        skill_path.as_path(),
    );
    let expected = expected_absolute_path(&skill_path);

    assert_eq!(path, expected);
}

#[test]
fn normalize_path_for_skill_id_repo_root_not_in_skill_path_uses_absolute_path() {
    let repo_root = PathBuf::from("/repo/root");
    let skill_path = PathBuf::from("/other/path/.codex/skills/doc/SKILL.md");

    let path = normalize_path_for_skill_id(
        Some("https://example.com/repo.git"),
        Some(repo_root.as_path()),
        skill_path.as_path(),
    );
    let expected = expected_absolute_path(&skill_path);

    assert_eq!(path, expected);
}

#[test]
fn app_mentioned_event_serializes_expected_shape() {
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-1".to_string(),
    };
    let event = TrackEventRequest::AppMentioned(CodexAppMentionedEventRequest {
        event_type: "codex_app_mentioned",
        event_params: codex_app_metadata(
            &tracking,
            AppInvocation {
                connector_id: Some("calendar".to_string()),
                app_name: Some("Calendar".to_string()),
                invocation_type: Some(InvocationType::Explicit),
            },
        ),
    });

    let payload = serde_json::to_value(&event).expect("serialize app mentioned event");

    assert_eq!(
        payload,
        json!({
            "event_type": "codex_app_mentioned",
            "event_params": {
                "connector_id": "calendar",
                "thread_id": "thread-1",
                "turn_id": "turn-1",
                "app_name": "Calendar",
                "product_client_id": originator().value,
                "invoke_type": "explicit",
                "model_slug": "gpt-5"
            }
        })
    );
}

#[test]
fn app_used_event_serializes_expected_shape() {
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-2".to_string(),
        turn_id: "turn-2".to_string(),
    };
    let event = TrackEventRequest::AppUsed(CodexAppUsedEventRequest {
        event_type: "codex_app_used",
        event_params: codex_app_metadata(
            &tracking,
            AppInvocation {
                connector_id: Some("drive".to_string()),
                app_name: Some("Google Drive".to_string()),
                invocation_type: Some(InvocationType::Implicit),
            },
        ),
    });

    let payload = serde_json::to_value(&event).expect("serialize app used event");

    assert_eq!(
        payload,
        json!({
            "event_type": "codex_app_used",
            "event_params": {
                "connector_id": "drive",
                "thread_id": "thread-2",
                "turn_id": "turn-2",
                "app_name": "Google Drive",
                "product_client_id": originator().value,
                "invoke_type": "implicit",
                "model_slug": "gpt-5"
            }
        })
    );
}

#[test]
fn accepted_line_fingerprints_event_serializes_expected_shape() {
    let event = TrackEventRequest::AcceptedLineFingerprints(Box::new(
        CodexAcceptedLineFingerprintsEventRequest {
            event_type: "codex_accepted_line_fingerprints",
            event_params: CodexAcceptedLineFingerprintsEventParams {
                event_type: "codex.accepted_line_fingerprints",
                turn_id: "turn-1".to_string(),
                thread_id: "thread-1".to_string(),
                product_surface: Some("codex".to_string()),
                model_slug: Some("gpt-5.1-codex".to_string()),
                completed_at: 1710000000,
                repo_hash: Some("repo-hash-1".to_string()),
                accepted_added_lines: 42,
                accepted_deleted_lines: 40,
                line_fingerprints: Vec::new(),
            },
        },
    ));

    let payload = serde_json::to_value(&event).expect("serialize accepted line fingerprints event");

    assert_eq!(
        payload,
        json!({
            "event_type": "codex_accepted_line_fingerprints",
            "event_params": {
                "event_type": "codex.accepted_line_fingerprints",
                "turn_id": "turn-1",
                "thread_id": "thread-1",
                "product_surface": "codex",
                "model_slug": "gpt-5.1-codex",
                "completed_at": 1710000000,
                "repo_hash": "repo-hash-1",
                "accepted_added_lines": 42,
                "accepted_deleted_lines": 40,
                "line_fingerprints": []
            }
        })
    );
}

#[tokio::test]
async fn reducer_emits_large_accepted_line_aggregates_without_fingerprints() {
    let mut reducer = AnalyticsReducer::default();
    let mut events = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut events,
        /*include_initialize*/ true,
        /*include_resolved_config*/ true,
        /*include_started*/ true,
        /*include_token_usage*/ true,
    )
    .await;
    events.clear();

    let mut diff = "\
diff --git a/src/lib.rs b/src/lib.rs
index 1111111..2222222
--- a/src/lib.rs
+++ b/src/lib.rs
@@ -0,0 +1,20000 @@
"
    .to_string();
    for index in 0..20_000 {
        diff.push_str(&format!("+let value_{index} = {index};\n"));
    }

    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(ServerNotification::TurnDiffUpdated(
                TurnDiffUpdatedNotification {
                    thread_id: "thread-2".to_string(),
                    turn_id: "turn-2".to_string(),
                    diff,
                },
            ))),
            &mut events,
        )
        .await;
    assert!(events.is_empty());

    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut events,
        )
        .await;

    let accepted_line_events = events
        .iter()
        .filter_map(|event| match event {
            TrackEventRequest::AcceptedLineFingerprints(event) => Some(event),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(accepted_line_events.len(), 1);
    let event = accepted_line_events[0];
    assert_eq!(event.event_params.turn_id, "turn-2");
    assert_eq!(event.event_params.thread_id, "thread-2");
    assert_eq!(event.event_params.accepted_added_lines, 20_000);
    assert_eq!(event.event_params.accepted_deleted_lines, 0);
    assert!(event.event_params.line_fingerprints.is_empty());
    assert!(serde_json::to_vec(event).expect("serialize event").len() < 2_100_000);
}

#[tokio::test]
async fn reducer_emits_accepted_line_fingerprints_once_from_latest_turn_diff_on_completion() {
    let mut reducer = AnalyticsReducer::default();
    let mut events = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut events,
        /*include_initialize*/ true,
        /*include_resolved_config*/ true,
        /*include_started*/ true,
        /*include_token_usage*/ true,
    )
    .await;
    events.clear();

    for line in ["let old_value = 1;", "let latest_value = 2;"] {
        let diff = format!(
            "\
diff --git a/src/lib.rs b/src/lib.rs
index 1111111..2222222
--- a/src/lib.rs
+++ b/src/lib.rs
@@ -0,0 +1 @@
+{line}
"
        );
        reducer
            .ingest(
                AnalyticsFact::Notification(Box::new(ServerNotification::TurnDiffUpdated(
                    TurnDiffUpdatedNotification {
                        thread_id: "thread-2".to_string(),
                        turn_id: "turn-2".to_string(),
                        diff,
                    },
                ))),
                &mut events,
            )
            .await;
    }
    assert!(events.is_empty());

    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut events,
        )
        .await;

    let accepted_line_events = events
        .iter()
        .filter_map(|event| match event {
            TrackEventRequest::AcceptedLineFingerprints(event) => Some(event),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(accepted_line_events.len(), 1);
    let event = accepted_line_events[0];
    assert_eq!(event.event_params.accepted_added_lines, 1);
    assert!(event.event_params.line_fingerprints.is_empty());
}

#[test]
fn compaction_event_serializes_expected_shape() {
    let event = TrackEventRequest::Compaction(Box::new(CodexCompactionEventRequest {
        event_type: "codex_compaction_event",
        event_params: crate::events::codex_compaction_event_params(
            CodexCompactionEvent {
                thread_id: "thread-1".to_string(),
                turn_id: "turn-1".to_string(),
                trigger: CompactionTrigger::Auto,
                reason: CompactionReason::ContextLimit,
                implementation: CompactionImplementation::ResponsesCompact,
                phase: CompactionPhase::MidTurn,
                strategy: CompactionStrategy::Memento,
                status: CompactionStatus::Completed,
                error: None,
                active_context_tokens_before: 120_000,
                active_context_tokens_after: 18_000,
                started_at: 100,
                completed_at: 106,
                duration_ms: Some(6543),
            },
            "session-thread-1".to_string(),
            sample_app_server_client_metadata(),
            sample_runtime_metadata(),
            Some(ThreadSource::User),
            /*subagent_source*/ None,
            /*parent_thread_id*/ None,
        ),
    }));

    let payload = serde_json::to_value(&event).expect("serialize compaction event");

    assert_eq!(
        payload,
        json!({
            "event_type": "codex_compaction_event",
            "event_params": {
                "thread_id": "thread-1",
                "session_id": "session-thread-1",
                "turn_id": "turn-1",
                "app_server_client": {
                    "product_client_id": DEFAULT_ORIGINATOR,
                    "client_name": "codex-tui",
                    "client_version": "1.0.0",
                    "rpc_transport": "stdio",
                    "experimental_api_enabled": true
                },
                "runtime": {
                    "codex_rs_version": "0.1.0",
                    "runtime_os": "macos",
                    "runtime_os_version": "15.3.1",
                    "runtime_arch": "aarch64"
                },
                "thread_source": "user",
                "subagent_source": null,
                "parent_thread_id": null,
                "trigger": "auto",
                "reason": "context_limit",
                "implementation": "responses_compact",
                "phase": "mid_turn",
                "strategy": "memento",
                "status": "completed",
                "error": null,
                "active_context_tokens_before": 120000,
                "active_context_tokens_after": 18000,
                "started_at": 100,
                "completed_at": 106,
                "duration_ms": 6543
            }
        })
    );
}

#[test]
fn compaction_implementation_serializes_remote_v2() {
    let payload = serde_json::to_value(CompactionImplementation::ResponsesCompactionV2)
        .expect("serialize compaction implementation");

    assert_eq!(payload, json!("responses_compaction_v2"));
}

#[test]
fn app_used_dedupe_is_keyed_by_turn_and_connector() {
    let (sender, _receiver) = mpsc::channel(1);
    let queue = AnalyticsEventsQueue {
        sender,
        app_used_emitted_keys: Arc::new(Mutex::new(HashSet::new())),
        plugin_used_emitted_keys: Arc::new(Mutex::new(HashSet::new())),
    };
    let app = AppInvocation {
        connector_id: Some("calendar".to_string()),
        app_name: Some("Calendar".to_string()),
        invocation_type: Some(InvocationType::Implicit),
    };

    let turn_1 = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-1".to_string(),
    };
    let turn_2 = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-2".to_string(),
    };

    assert_eq!(queue.should_enqueue_app_used(&turn_1, &app), true);
    assert_eq!(queue.should_enqueue_app_used(&turn_1, &app), false);
    assert_eq!(queue.should_enqueue_app_used(&turn_2, &app), true);
}

#[test]
fn thread_initialized_event_serializes_expected_shape() {
    let event = TrackEventRequest::ThreadInitialized(ThreadInitializedEvent {
        event_type: "codex_thread_initialized",
        event_params: ThreadInitializedEventParams {
            thread_id: "thread-0".to_string(),
            app_server_client: CodexAppServerClientMetadata {
                product_client_id: DEFAULT_ORIGINATOR.to_string(),
                client_name: Some("codex-tui".to_string()),
                client_version: Some("1.0.0".to_string()),
                rpc_transport: AppServerRpcTransport::Stdio,
                experimental_api_enabled: Some(true),
            },
            runtime: CodexRuntimeMetadata {
                codex_rs_version: "0.1.0".to_string(),
                runtime_os: "macos".to_string(),
                runtime_os_version: "15.3.1".to_string(),
                runtime_arch: "aarch64".to_string(),
            },
            model: "gpt-5".to_string(),
            ephemeral: true,
            thread_source: Some("user"),
            initialization_mode: ThreadInitializationMode::New,
            subagent_source: None,
            parent_thread_id: None,
            created_at: 1,
        },
    });

    let payload = serde_json::to_value(&event).expect("serialize thread initialized event");

    assert_eq!(
        payload,
        json!({
            "event_type": "codex_thread_initialized",
            "event_params": {
                "thread_id": "thread-0",
                "app_server_client": {
                    "product_client_id": DEFAULT_ORIGINATOR,
                    "client_name": "codex-tui",
                    "client_version": "1.0.0",
                    "rpc_transport": "stdio",
                    "experimental_api_enabled": true
                },
                "runtime": {
                    "codex_rs_version": "0.1.0",
                    "runtime_os": "macos",
                    "runtime_os_version": "15.3.1",
                    "runtime_arch": "aarch64"
                },
                "model": "gpt-5",
                "ephemeral": true,
                "thread_source": "user",
                "initialization_mode": "new",
                "subagent_source": null,
                "parent_thread_id": null,
                "created_at": 1
            }
        })
    );
}

#[tokio::test]
async fn initialize_caches_client_and_thread_lifecycle_publishes_once_initialized() {
    let mut reducer = AnalyticsReducer::default();
    let mut events = Vec::new();

    reducer
        .ingest(
            AnalyticsFact::Response {
                connection_id: 7,
                response: Box::new(sample_thread_start_response(
                    "thread-no-client",
                    /*ephemeral*/ false,
                    "gpt-5",
                )),
            },
            &mut events,
        )
        .await;
    assert!(events.is_empty(), "thread events should require initialize");

    reducer
        .ingest(
            AnalyticsFact::Initialize {
                connection_id: 7,
                params: InitializeParams {
                    client_info: ClientInfo {
                        name: "codex-tui".to_string(),
                        title: None,
                        version: "1.0.0".to_string(),
                    },
                    capabilities: Some(InitializeCapabilities {
                        experimental_api: false,
                        opt_out_notification_methods: None,
                    }),
                },
                product_client_id: DEFAULT_ORIGINATOR.to_string(),
                runtime: CodexRuntimeMetadata {
                    codex_rs_version: "0.99.0".to_string(),
                    runtime_os: "linux".to_string(),
                    runtime_os_version: "24.04".to_string(),
                    runtime_arch: "x86_64".to_string(),
                },
                rpc_transport: AppServerRpcTransport::Websocket,
            },
            &mut events,
        )
        .await;
    assert!(events.is_empty(), "initialize should not publish by itself");

    reducer
        .ingest(
            AnalyticsFact::Response {
                connection_id: 7,
                response: Box::new(sample_thread_resume_response(
                    "thread-1", /*ephemeral*/ true, "gpt-5",
                )),
            },
            &mut events,
        )
        .await;

    let payload = serde_json::to_value(&events).expect("serialize events");
    assert_eq!(payload.as_array().expect("events array").len(), 1);
    assert_eq!(payload[0]["event_type"], "codex_thread_initialized");
    assert_eq!(
        payload[0]["event_params"]["app_server_client"]["product_client_id"],
        DEFAULT_ORIGINATOR
    );
    assert_eq!(
        payload[0]["event_params"]["app_server_client"]["client_name"],
        "codex-tui"
    );
    assert_eq!(
        payload[0]["event_params"]["app_server_client"]["client_version"],
        "1.0.0"
    );
    assert_eq!(
        payload[0]["event_params"]["app_server_client"]["rpc_transport"],
        "websocket"
    );
    assert_eq!(
        payload[0]["event_params"]["app_server_client"]["experimental_api_enabled"],
        false
    );
    assert_eq!(
        payload[0]["event_params"]["runtime"]["codex_rs_version"],
        "0.99.0"
    );
    assert_eq!(payload[0]["event_params"]["runtime"]["runtime_os"], "linux");
    assert_eq!(
        payload[0]["event_params"]["runtime"]["runtime_os_version"],
        "24.04"
    );
    assert_eq!(
        payload[0]["event_params"]["runtime"]["runtime_arch"],
        "x86_64"
    );
    assert_eq!(payload[0]["event_params"]["initialization_mode"], "resumed");
    assert_eq!(payload[0]["event_params"]["thread_source"], "user");
    assert_eq!(payload[0]["event_params"]["subagent_source"], json!(null));
    assert_eq!(payload[0]["event_params"]["parent_thread_id"], json!(null));
}

#[test]
fn plugin_used_event_serializes_expected_shape() {
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-3".to_string(),
        turn_id: "turn-3".to_string(),
    };
    let event = TrackEventRequest::PluginUsed(CodexPluginUsedEventRequest {
        event_type: "codex_plugin_used",
        event_params: codex_plugin_used_metadata(&tracking, sample_plugin_metadata()),
    });

    let payload = serde_json::to_value(&event).expect("serialize plugin used event");

    assert_eq!(
        payload,
        json!({
            "event_type": "codex_plugin_used",
            "event_params": {
                "plugin_id": "sample@test",
                "plugin_name": "sample",
                "marketplace_name": "test",
                "has_skills": true,
                "mcp_server_count": 2,
                "connector_ids": ["calendar", "drive"],
                "product_client_id": originator().value,
                "thread_id": "thread-3",
                "turn_id": "turn-3",
                "model_slug": "gpt-5"
            }
        })
    );
}

#[test]
fn plugin_management_event_serializes_expected_shape() {
    let event = TrackEventRequest::PluginInstalled(CodexPluginEventRequest {
        event_type: "codex_plugin_installed",
        event_params: codex_plugin_metadata(sample_plugin_metadata()),
    });

    let payload = serde_json::to_value(&event).expect("serialize plugin installed event");

    assert_eq!(
        payload,
        json!({
            "event_type": "codex_plugin_installed",
            "event_params": {
                "plugin_id": "sample@test",
                "plugin_name": "sample",
                "marketplace_name": "test",
                "has_skills": true,
                "mcp_server_count": 2,
                "connector_ids": ["calendar", "drive"],
                "product_client_id": originator().value
            }
        })
    );
}

#[test]
fn plugin_management_event_can_use_remote_plugin_id_override() {
    let mut plugin = sample_plugin_metadata();
    plugin.remote_plugin_id = Some("plugins~Plugin_remote".to_string());
    let event = TrackEventRequest::PluginInstalled(CodexPluginEventRequest {
        event_type: "codex_plugin_installed",
        event_params: codex_plugin_metadata(plugin),
    });

    let payload = serde_json::to_value(&event).expect("serialize plugin installed event");

    assert_eq!(
        payload["event_params"]["plugin_id"],
        "plugins~Plugin_remote"
    );
    assert_eq!(payload["event_params"]["plugin_name"], "sample");
    assert_eq!(payload["event_params"]["marketplace_name"], "test");
}

#[test]
fn hook_run_event_serializes_expected_shape() {
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-3".to_string(),
        turn_id: "turn-3".to_string(),
    };
    let event = TrackEventRequest::HookRun(CodexHookRunEventRequest {
        event_type: "codex_hook_run",
        event_params: codex_hook_run_metadata(
            &tracking,
            HookRunFact {
                event_name: HookEventName::PreToolUse,
                hook_source: HookSource::User,
                status: HookRunStatus::Completed,
            },
        ),
    });

    let payload = serde_json::to_value(&event).expect("serialize hook run event");

    assert_eq!(
        payload,
        json!({
            "event_type": "codex_hook_run",
            "event_params": {
                "thread_id": "thread-3",
                "turn_id": "turn-3",
                "model_slug": "gpt-5",
                "hook_name": "PreToolUse",
                "hook_source": "user",
                "status": "completed"
            }
        })
    );
}

#[test]
fn hook_run_metadata_maps_sources_and_statuses() {
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-1".to_string(),
    };

    let system = serde_json::to_value(codex_hook_run_metadata(
        &tracking,
        HookRunFact {
            event_name: HookEventName::SessionStart,
            hook_source: HookSource::System,
            status: HookRunStatus::Completed,
        },
    ))
    .expect("serialize system hook");
    let project = serde_json::to_value(codex_hook_run_metadata(
        &tracking,
        HookRunFact {
            event_name: HookEventName::Stop,
            hook_source: HookSource::Project,
            status: HookRunStatus::Blocked,
        },
    ))
    .expect("serialize project hook");
    let cloud_requirements = serde_json::to_value(codex_hook_run_metadata(
        &tracking,
        HookRunFact {
            event_name: HookEventName::Stop,
            hook_source: HookSource::CloudRequirements,
            status: HookRunStatus::Blocked,
        },
    ))
    .expect("serialize cloud requirements hook");
    let unknown = serde_json::to_value(codex_hook_run_metadata(
        &tracking,
        HookRunFact {
            event_name: HookEventName::UserPromptSubmit,
            hook_source: HookSource::Unknown,
            status: HookRunStatus::Failed,
        },
    ))
    .expect("serialize unknown hook");

    assert_eq!(system["hook_source"], "system");
    assert_eq!(system["status"], "completed");
    assert_eq!(project["hook_source"], "project");
    assert_eq!(project["status"], "blocked");
    assert_eq!(cloud_requirements["hook_source"], "cloud_requirements");
    assert_eq!(cloud_requirements["status"], "blocked");
    assert_eq!(unknown["hook_source"], "unknown");
    assert_eq!(unknown["status"], "failed");
}

#[test]
fn hook_run_metadata_maps_stopped_status() {
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-1".to_string(),
    };

    let stopped = serde_json::to_value(codex_hook_run_metadata(
        &tracking,
        HookRunFact {
            event_name: HookEventName::Stop,
            hook_source: HookSource::User,
            status: HookRunStatus::Stopped,
        },
    ))
    .expect("serialize stopped hook");

    assert_eq!(stopped["hook_source"], "user");
    assert_eq!(stopped["status"], "stopped");
}

#[test]
fn plugin_used_dedupe_is_keyed_by_turn_and_plugin() {
    let (sender, _receiver) = mpsc::channel(1);
    let queue = AnalyticsEventsQueue {
        sender,
        app_used_emitted_keys: Arc::new(Mutex::new(HashSet::new())),
        plugin_used_emitted_keys: Arc::new(Mutex::new(HashSet::new())),
    };
    let plugin = sample_plugin_metadata();

    let turn_1 = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-1".to_string(),
    };
    let turn_2 = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-2".to_string(),
    };

    assert_eq!(queue.should_enqueue_plugin_used(&turn_1, &plugin), true);
    assert_eq!(queue.should_enqueue_plugin_used(&turn_1, &plugin), false);
    assert_eq!(queue.should_enqueue_plugin_used(&turn_2, &plugin), true);
}

#[tokio::test]
async fn reducer_ingests_skill_invoked_fact() {
    let mut reducer = AnalyticsReducer::default();
    let mut events = Vec::new();
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-1".to_string(),
    };
    let skill_path = PathBuf::from("/Users/abc/.codex/skills/doc/SKILL.md");
    let expected_skill_id = skill_id_for_local_skill(
        /*repo_url*/ None,
        /*repo_root*/ None,
        skill_path.as_path(),
        "doc",
    );

    reducer
        .ingest(
            AnalyticsFact::Custom(CustomAnalyticsFact::SkillInvoked(SkillInvokedInput {
                tracking,
                invocations: vec![SkillInvocation {
                    skill_name: "doc".to_string(),
                    skill_scope: codex_protocol::protocol::SkillScope::User,
                    skill_path,
                    plugin_id: None,
                    invocation_type: InvocationType::Explicit,
                }],
            })),
            &mut events,
        )
        .await;

    let payload = serde_json::to_value(&events).expect("serialize events");
    assert_eq!(
        payload,
        json!([{
            "event_type": "skill_invocation",
            "skill_id": expected_skill_id,
            "skill_name": "doc",
            "event_params": {
                "product_client_id": originator().value,
                "skill_scope": "user",
                "plugin_id": null,
                "repo_url": null,
                "thread_id": "thread-1",
                "turn_id": "turn-1",
                "invoke_type": "explicit",
                "model_slug": "gpt-5"
            }
        }])
    );
}

#[tokio::test]
async fn reducer_includes_plugin_id_for_plugin_skill_invocations() {
    let mut reducer = AnalyticsReducer::default();
    let mut events = Vec::new();
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-1".to_string(),
    };
    let skill_path =
        PathBuf::from("/Users/abc/.codex/plugins/cache/test/sample/skills/doc/SKILL.md");

    reducer
        .ingest(
            AnalyticsFact::Custom(CustomAnalyticsFact::SkillInvoked(SkillInvokedInput {
                tracking,
                invocations: vec![SkillInvocation {
                    skill_name: "sample:doc".to_string(),
                    skill_scope: codex_protocol::protocol::SkillScope::User,
                    skill_path,
                    plugin_id: Some("sample@test".to_string()),
                    invocation_type: InvocationType::Explicit,
                }],
            })),
            &mut events,
        )
        .await;

    let payload = serde_json::to_value(&events).expect("serialize events");
    assert_eq!(
        payload[0]["event_params"]["plugin_id"],
        json!("sample@test")
    );
}

#[tokio::test]
async fn reducer_ingests_hook_run_fact() {
    let mut reducer = AnalyticsReducer::default();
    let mut events = Vec::new();

    reducer
        .ingest(
            AnalyticsFact::Custom(CustomAnalyticsFact::HookRun(HookRunInput {
                tracking: TrackEventsContext {
                    model_slug: "gpt-5".to_string(),
                    thread_id: "thread-1".to_string(),
                    turn_id: "turn-1".to_string(),
                },
                hook: HookRunFact {
                    event_name: HookEventName::PostToolUse,
                    hook_source: HookSource::Unknown,
                    status: HookRunStatus::Failed,
                },
            })),
            &mut events,
        )
        .await;

    let payload = serde_json::to_value(&events).expect("serialize events");
    assert_eq!(payload.as_array().expect("events array").len(), 1);
    assert_eq!(payload[0]["event_type"], "codex_hook_run");
    assert_eq!(payload[0]["event_params"]["hook_name"], "PostToolUse");
    assert_eq!(payload[0]["event_params"]["hook_source"], "unknown");
    assert_eq!(payload[0]["event_params"]["status"], "failed");
}

#[tokio::test]
async fn reducer_ingests_app_and_plugin_facts() {
    let mut reducer = AnalyticsReducer::default();
    let mut events = Vec::new();
    let tracking = TrackEventsContext {
        model_slug: "gpt-5".to_string(),
        thread_id: "thread-1".to_string(),
        turn_id: "turn-1".to_string(),
    };

    reducer
        .ingest(
            AnalyticsFact::Custom(CustomAnalyticsFact::AppMentioned(AppMentionedInput {
                tracking: tracking.clone(),
                mentions: vec![AppInvocation {
                    connector_id: Some("calendar".to_string()),
                    app_name: Some("Calendar".to_string()),
                    invocation_type: Some(InvocationType::Explicit),
                }],
            })),
            &mut events,
        )
        .await;
    reducer
        .ingest(
            AnalyticsFact::Custom(CustomAnalyticsFact::AppUsed(AppUsedInput {
                tracking: tracking.clone(),
                app: AppInvocation {
                    connector_id: Some("drive".to_string()),
                    app_name: Some("Drive".to_string()),
                    invocation_type: Some(InvocationType::Implicit),
                },
            })),
            &mut events,
        )
        .await;
    reducer
        .ingest(
            AnalyticsFact::Custom(CustomAnalyticsFact::PluginUsed(PluginUsedInput {
                tracking,
                plugin: sample_plugin_metadata(),
            })),
            &mut events,
        )
        .await;

    let payload = serde_json::to_value(&events).expect("serialize events");
    assert_eq!(payload.as_array().expect("events array").len(), 3);
    assert_eq!(payload[0]["event_type"], "codex_app_mentioned");
    assert_eq!(payload[1]["event_type"], "codex_app_used");
    assert_eq!(payload[2]["event_type"], "codex_plugin_used");
}

#[tokio::test]
async fn reducer_ingests_plugin_state_changed_fact() {
    let mut reducer = AnalyticsReducer::default();
    let mut events = Vec::new();

    reducer
        .ingest(
            AnalyticsFact::Custom(CustomAnalyticsFact::PluginStateChanged(
                PluginStateChangedInput {
                    plugin: sample_plugin_metadata(),
                    state: PluginState::Disabled,
                },
            )),
            &mut events,
        )
        .await;

    let payload = serde_json::to_value(&events).expect("serialize events");
    assert_eq!(
        payload,
        json!([{
            "event_type": "codex_plugin_disabled",
            "event_params": {
                "plugin_id": "sample@test",
                "plugin_name": "sample",
                "marketplace_name": "test",
                "has_skills": true,
                "mcp_server_count": 2,
                "connector_ids": ["calendar", "drive"],
                "product_client_id": originator().value
            }
        }])
    );
}

#[test]
fn turn_event_serializes_expected_shape() {
    let event = TrackEventRequest::TurnEvent(Box::new(CodexTurnEventRequest {
        event_type: "codex_turn_event",
        event_params: crate::events::CodexTurnEventParams {
            thread_id: "thread-2".to_string(),
            session_id: "session-thread-2".to_string(),
            turn_id: "turn-2".to_string(),
            app_server_client: sample_app_server_client_metadata(),
            runtime: sample_runtime_metadata(),
            submission_type: None,
            ephemeral: false,
            thread_source: Some(ThreadSource::User),
            initialization_mode: ThreadInitializationMode::New,
            subagent_source: None,
            parent_thread_id: None,
            model: Some("gpt-5".to_string()),
            model_provider: "openai".to_string(),
            sandbox_policy: Some("read_only"),
            reasoning_effort: Some("high".to_string()),
            reasoning_summary: Some("detailed".to_string()),
            service_tier: "flex".to_string(),
            approval_policy: "on-request".to_string(),
            approvals_reviewer: "auto_review".to_string(),
            sandbox_network_access: true,
            collaboration_mode: Some("plan"),
            personality: Some("pragmatic".to_string()),
            num_input_images: 2,
            is_first_turn: true,
            status: Some(TurnStatus::Completed),
            turn_error: None,
            steer_count: Some(0),
            total_tool_call_count: None,
            shell_command_count: None,
            file_change_count: None,
            mcp_tool_call_count: None,
            dynamic_tool_call_count: None,
            subagent_tool_call_count: None,
            web_search_count: None,
            image_generation_count: None,
            input_tokens: None,
            cached_input_tokens: None,
            output_tokens: None,
            reasoning_output_tokens: None,
            total_tokens: None,
            duration_ms: Some(1234),
            started_at: Some(455),
            completed_at: Some(456),
        },
    }));

    let payload = serde_json::to_value(&event).expect("serialize turn event");
    let expected = serde_json::from_str::<serde_json::Value>(
        r#"{
            "event_type": "codex_turn_event",
            "event_params": {
                "thread_id": "thread-2",
                "session_id": "session-thread-2",
                "turn_id": "turn-2",
                "submission_type": null,
                "app_server_client": {
                    "product_client_id": "codex_cli_rs",
                    "client_name": "codex-tui",
                    "client_version": "1.0.0",
                    "rpc_transport": "stdio",
                    "experimental_api_enabled": true
                },
                "runtime": {
                    "codex_rs_version": "0.1.0",
                    "runtime_os": "macos",
                    "runtime_os_version": "15.3.1",
                    "runtime_arch": "aarch64"
                },
                "ephemeral": false,
                "thread_source": "user",
                "initialization_mode": "new",
                "subagent_source": null,
                "parent_thread_id": null,
                "model": "gpt-5",
                "model_provider": "openai",
                "sandbox_policy": "read_only",
                "reasoning_effort": "high",
                "reasoning_summary": "detailed",
                "service_tier": "flex",
                "approval_policy": "on-request",
                "approvals_reviewer": "auto_review",
                "sandbox_network_access": true,
                "collaboration_mode": "plan",
                "personality": "pragmatic",
                "num_input_images": 2,
                "is_first_turn": true,
                "status": "completed",
                "turn_error": null,
                "steer_count": 0,
                "total_tool_call_count": null,
                "shell_command_count": null,
                "file_change_count": null,
                "mcp_tool_call_count": null,
                "dynamic_tool_call_count": null,
                "subagent_tool_call_count": null,
                "web_search_count": null,
                "image_generation_count": null,
                "input_tokens": null,
                "cached_input_tokens": null,
                "output_tokens": null,
                "reasoning_output_tokens": null,
                "total_tokens": null,
                "duration_ms": 1234,
                "started_at": 455,
                "completed_at": 456
            }
        }"#,
    )
    .expect("parse expected turn event");

    assert_eq!(payload, expected);
}

#[tokio::test]
async fn accepted_turn_steer_emits_expected_event() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ true,
        /*include_resolved_config*/ false,
        /*include_started*/ false,
        /*include_token_usage*/ false,
    )
    .await;
    reducer
        .ingest(
            AnalyticsFact::ClientRequest {
                connection_id: 7,
                request_id: RequestId::Integer(4),
                request: Box::new(sample_turn_steer_request(
                    "thread-2", "turn-2", /*request_id*/ 4,
                )),
            },
            &mut out,
        )
        .await;
    reducer
        .ingest(
            AnalyticsFact::ClientResponse {
                connection_id: 7,
                request_id: RequestId::Integer(4),
                response: Box::new(sample_turn_steer_response("turn-2")),
            },
            &mut out,
        )
        .await;

    assert_eq!(out.len(), 1);
    let payload = serde_json::to_value(&out[0]).expect("serialize turn steer event");
    assert_eq!(payload["event_type"], json!("codex_turn_steer_event"));
    assert_eq!(payload["event_params"]["thread_id"], json!("thread-2"));
    assert_eq!(
        payload["event_params"]["session_id"],
        json!("session-thread-2")
    );
    assert_eq!(payload["event_params"]["expected_turn_id"], json!("turn-2"));
    assert_eq!(payload["event_params"]["accepted_turn_id"], json!("turn-2"));
    assert_eq!(payload["event_params"]["num_input_images"], json!(1));
    assert_eq!(payload["event_params"]["result"], json!("accepted"));
    assert_eq!(payload["event_params"]["rejection_reason"], json!(null));
    assert!(
        payload["event_params"]["created_at"]
            .as_u64()
            .expect("created_at")
            > 0
    );
    assert_eq!(
        payload["event_params"]["app_server_client"]["product_client_id"],
        json!("codex-tui")
    );
    assert_eq!(
        payload["event_params"]["runtime"]["codex_rs_version"],
        json!("0.1.0")
    );
    assert_eq!(payload["event_params"]["thread_source"], json!("user"));
    assert_eq!(payload["event_params"]["subagent_source"], json!(null));
    assert_eq!(payload["event_params"]["parent_thread_id"], json!(null));
    assert!(payload["event_params"].get("product_client_id").is_none());
}

#[tokio::test]
async fn rejected_turn_steer_uses_request_connection_metadata() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();
    let payload = ingest_rejected_turn_steer(
        &mut reducer,
        &mut out,
        no_active_turn_steer_error(),
        Some(no_active_turn_steer_error_type()),
    )
    .await;

    assert_eq!(payload["event_type"], json!("codex_turn_steer_event"));
    assert_eq!(payload["event_params"]["thread_id"], json!("thread-2"));
    assert_eq!(payload["event_params"]["expected_turn_id"], json!("turn-2"));
    assert_eq!(payload["event_params"]["accepted_turn_id"], json!(null));
    assert_eq!(payload["event_params"]["num_input_images"], json!(1));
    assert_eq!(
        payload["event_params"]["app_server_client"]["product_client_id"],
        json!("codex-tui")
    );
    assert_eq!(
        payload["event_params"]["runtime"]["codex_rs_version"],
        json!("0.1.0")
    );
    assert_eq!(payload["event_params"]["thread_source"], json!("user"));
    assert_eq!(payload["event_params"]["subagent_source"], json!(null));
    assert_eq!(payload["event_params"]["parent_thread_id"], json!(null));
    assert_eq!(payload["event_params"]["result"], json!("rejected"));
    assert_eq!(
        payload["event_params"]["rejection_reason"],
        json!("no_active_turn")
    );
    assert!(
        payload["event_params"]["created_at"]
            .as_u64()
            .expect("created_at")
            > 0
    );
}

#[tokio::test]
async fn rejected_turn_steer_maps_active_turn_not_steerable_error_type() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();
    let payload = ingest_rejected_turn_steer(
        &mut reducer,
        &mut out,
        non_steerable_review_error(),
        Some(non_steerable_review_error_type()),
    )
    .await;

    assert_eq!(
        payload["event_params"]["rejection_reason"],
        json!("non_steerable_review")
    );
}

#[tokio::test]
async fn rejected_turn_steer_maps_input_too_large_error_type() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();
    let payload = ingest_rejected_turn_steer(
        &mut reducer,
        &mut out,
        input_too_large_steer_error(),
        Some(input_too_large_error_type()),
    )
    .await;

    assert_eq!(
        payload["event_params"]["rejection_reason"],
        json!("input_too_large")
    );
}

#[tokio::test]
async fn turn_steer_does_not_emit_without_pending_request() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    reducer
        .ingest(
            AnalyticsFact::ErrorResponse {
                connection_id: 7,
                request_id: RequestId::Integer(4),
                error: no_active_turn_steer_error(),
                error_type: Some(no_active_turn_steer_error_type()),
            },
            &mut out,
        )
        .await;

    assert!(out.is_empty());
}

#[tokio::test]
async fn turn_start_error_response_discards_pending_start_request() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_initialize(&mut reducer, &mut out).await;
    reducer
        .ingest(
            AnalyticsFact::ClientRequest {
                connection_id: 7,
                request_id: RequestId::Integer(3),
                request: Box::new(sample_turn_start_request("thread-2", /*request_id*/ 3)),
            },
            &mut out,
        )
        .await;
    reducer
        .ingest(
            AnalyticsFact::ErrorResponse {
                connection_id: 7,
                request_id: RequestId::Integer(3),
                error: no_active_turn_steer_error(),
                error_type: None,
            },
            &mut out,
        )
        .await;

    // A late/synthetic response for the same request id must not resurrect the
    // failed turn/start request and attach request-scoped connection metadata.
    reducer
        .ingest(
            AnalyticsFact::ClientResponse {
                connection_id: 7,
                request_id: RequestId::Integer(3),
                response: Box::new(sample_turn_start_response("turn-2")),
            },
            &mut out,
        )
        .await;
    assert!(out.is_empty());

    reducer
        .ingest(
            AnalyticsFact::Custom(CustomAnalyticsFact::TurnResolvedConfig(Box::new(
                sample_turn_resolved_config("thread-2", "turn-2"),
            ))),
            &mut out,
        )
        .await;
    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;

    assert!(out.is_empty());
}

#[tokio::test]
async fn turn_lifecycle_emits_turn_event() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ true,
        /*include_resolved_config*/ true,
        /*include_started*/ true,
        /*include_token_usage*/ true,
    )
    .await;
    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;

    assert_eq!(out.len(), 1);
    let payload = serde_json::to_value(&out[0]).expect("serialize turn event");
    assert_eq!(payload["event_type"], json!("codex_turn_event"));
    assert_eq!(payload["event_params"]["thread_id"], json!("thread-2"));
    assert_eq!(
        payload["event_params"]["session_id"],
        json!("session-thread-2")
    );
    assert_eq!(payload["event_params"]["turn_id"], json!("turn-2"));
    assert_eq!(
        payload["event_params"]["app_server_client"],
        json!({
            "product_client_id": "codex-tui",
            "client_name": "codex-tui",
            "client_version": "1.0.0",
            "rpc_transport": "stdio",
            "experimental_api_enabled": null,
        })
    );
    assert_eq!(
        payload["event_params"]["runtime"],
        json!({
            "codex_rs_version": "0.1.0",
            "runtime_os": "macos",
            "runtime_os_version": "15.3.1",
            "runtime_arch": "aarch64",
        })
    );
    assert!(payload["event_params"].get("product_client_id").is_none());
    assert_eq!(payload["event_params"]["ephemeral"], json!(false));
    assert_eq!(payload["event_params"]["num_input_images"], json!(1));
    assert_eq!(payload["event_params"]["status"], json!("completed"));
    assert_eq!(payload["event_params"]["steer_count"], json!(0));
    assert_eq!(payload["event_params"]["total_tool_call_count"], json!(0));
    assert_eq!(payload["event_params"]["shell_command_count"], json!(0));
    assert_eq!(payload["event_params"]["file_change_count"], json!(0));
    assert_eq!(payload["event_params"]["mcp_tool_call_count"], json!(0));
    assert_eq!(payload["event_params"]["dynamic_tool_call_count"], json!(0));
    assert_eq!(
        payload["event_params"]["subagent_tool_call_count"],
        json!(0)
    );
    assert_eq!(payload["event_params"]["web_search_count"], json!(0));
    assert_eq!(payload["event_params"]["image_generation_count"], json!(0));
    assert_eq!(payload["event_params"]["started_at"], json!(455));
    assert_eq!(payload["event_params"]["completed_at"], json!(456));
    assert_eq!(payload["event_params"]["duration_ms"], json!(1234));
    assert_eq!(payload["event_params"]["input_tokens"], json!(123));
    assert_eq!(payload["event_params"]["cached_input_tokens"], json!(45));
    assert_eq!(payload["event_params"]["output_tokens"], json!(140));
    assert_eq!(
        payload["event_params"]["reasoning_output_tokens"],
        json!(13)
    );
    assert_eq!(payload["event_params"]["total_tokens"], json!(321));
}

#[tokio::test]
async fn turn_event_counts_completed_tool_items() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ true,
        /*include_resolved_config*/ true,
        /*include_started*/ true,
        /*include_token_usage*/ false,
    )
    .await;

    let completed_tool_items = vec![
        sample_command_execution_item(CommandExecutionStatus::Completed, Some(0), Some(1)),
        ThreadItem::FileChange {
            id: "file-change-1".to_string(),
            changes: Vec::new(),
            status: PatchApplyStatus::Completed,
        },
        ThreadItem::McpToolCall {
            id: "mcp-1".to_string(),
            server: "server".to_string(),
            tool: "search".to_string(),
            status: McpToolCallStatus::Completed,
            arguments: json!({}),
            mcp_app_resource_uri: None,
            plugin_id: None,
            result: None,
            error: None,
            duration_ms: Some(2),
        },
        ThreadItem::DynamicToolCall {
            id: "dynamic-1".to_string(),
            namespace: None,
            tool: "render".to_string(),
            arguments: json!({}),
            status: DynamicToolCallStatus::Completed,
            content_items: None,
            success: Some(true),
            duration_ms: Some(3),
        },
        ThreadItem::CollabAgentToolCall {
            id: "collab-1".to_string(),
            tool: CollabAgentTool::SpawnAgent,
            status: CollabAgentToolCallStatus::Completed,
            sender_thread_id: "thread-2".to_string(),
            receiver_thread_ids: vec!["thread-child".to_string()],
            prompt: Some("help".to_string()),
            model: Some("gpt-5".to_string()),
            reasoning_effort: None,
            agents_states: Default::default(),
        },
        ThreadItem::WebSearch {
            id: "web-1".to_string(),
            query: "codex".to_string(),
            action: None,
        },
        ThreadItem::ImageGeneration {
            id: "image-1".to_string(),
            status: "completed".to_string(),
            revised_prompt: None,
            result: "ok".to_string(),
            saved_path: None,
        },
    ];

    for item in completed_tool_items {
        reducer
            .ingest(
                AnalyticsFact::Notification(Box::new(ServerNotification::ItemCompleted(
                    ItemCompletedNotification {
                        thread_id: "thread-2".to_string(),
                        turn_id: "turn-2".to_string(),
                        completed_at_ms: 1_000,
                        item,
                    },
                ))),
                &mut out,
            )
            .await;
    }

    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;

    let turn_event = out
        .iter()
        .find(|event| matches!(event, TrackEventRequest::TurnEvent(_)))
        .expect("turn event should be emitted");
    let payload = serde_json::to_value(turn_event).expect("serialize turn event");
    assert_eq!(payload["event_params"]["total_tool_call_count"], json!(7));
    assert_eq!(payload["event_params"]["shell_command_count"], json!(1));
    assert_eq!(payload["event_params"]["file_change_count"], json!(1));
    assert_eq!(payload["event_params"]["mcp_tool_call_count"], json!(1));
    assert_eq!(payload["event_params"]["dynamic_tool_call_count"], json!(1));
    assert_eq!(
        payload["event_params"]["subagent_tool_call_count"],
        json!(1)
    );
    assert_eq!(payload["event_params"]["web_search_count"], json!(1));
    assert_eq!(payload["event_params"]["image_generation_count"], json!(1));
}

#[tokio::test]
async fn item_completed_without_turn_state_does_not_create_turn_state() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(ServerNotification::ItemCompleted(
                ItemCompletedNotification {
                    thread_id: "thread-2".to_string(),
                    turn_id: "turn-2".to_string(),
                    completed_at_ms: 1_000,
                    item: sample_command_execution_item(
                        CommandExecutionStatus::Completed,
                        Some(0),
                        Some(1),
                    ),
                },
            ))),
            &mut out,
        )
        .await;

    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;

    assert!(out.is_empty());
}

#[tokio::test]
async fn accepted_steers_increment_turn_steer_count() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ true,
        /*include_resolved_config*/ true,
        /*include_started*/ true,
        /*include_token_usage*/ false,
    )
    .await;

    reducer
        .ingest(
            AnalyticsFact::ClientRequest {
                connection_id: 7,
                request_id: RequestId::Integer(4),
                request: Box::new(sample_turn_steer_request(
                    "thread-2", "turn-2", /*request_id*/ 4,
                )),
            },
            &mut out,
        )
        .await;
    reducer
        .ingest(
            AnalyticsFact::ClientResponse {
                connection_id: 7,
                request_id: RequestId::Integer(4),
                response: Box::new(sample_turn_steer_response("turn-2")),
            },
            &mut out,
        )
        .await;

    reducer
        .ingest(
            AnalyticsFact::ClientRequest {
                connection_id: 7,
                request_id: RequestId::Integer(5),
                request: Box::new(sample_turn_steer_request(
                    "thread-2", "turn-2", /*request_id*/ 5,
                )),
            },
            &mut out,
        )
        .await;
    reducer
        .ingest(
            AnalyticsFact::ErrorResponse {
                connection_id: 7,
                request_id: RequestId::Integer(5),
                error: no_active_turn_steer_error(),
                error_type: Some(no_active_turn_steer_error_type()),
            },
            &mut out,
        )
        .await;

    reducer
        .ingest(
            AnalyticsFact::ClientRequest {
                connection_id: 7,
                request_id: RequestId::Integer(6),
                request: Box::new(sample_turn_steer_request(
                    "thread-2", "turn-2", /*request_id*/ 6,
                )),
            },
            &mut out,
        )
        .await;
    reducer
        .ingest(
            AnalyticsFact::ClientResponse {
                connection_id: 7,
                request_id: RequestId::Integer(6),
                response: Box::new(sample_turn_steer_response("turn-2")),
            },
            &mut out,
        )
        .await;

    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;

    let turn_event = out
        .iter()
        .find(|event| matches!(event, TrackEventRequest::TurnEvent(_)))
        .expect("turn event should be emitted");
    let payload = serde_json::to_value(turn_event).expect("serialize turn event");
    assert_eq!(payload["event_params"]["steer_count"], json!(2));
}

#[tokio::test]
async fn turn_does_not_emit_without_required_prerequisites() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ false,
        /*include_resolved_config*/ true,
        /*include_started*/ false,
        /*include_token_usage*/ false,
    )
    .await;
    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;
    assert!(out.is_empty());

    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ true,
        /*include_resolved_config*/ false,
        /*include_started*/ false,
        /*include_token_usage*/ false,
    )
    .await;
    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;
    assert!(out.is_empty());
}

#[tokio::test]
async fn turn_lifecycle_emits_failed_turn_event() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ true,
        /*include_resolved_config*/ true,
        /*include_started*/ true,
        /*include_token_usage*/ false,
    )
    .await;
    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Failed,
                Some(codex_app_server_protocol::CodexErrorInfo::BadRequest),
            ))),
            &mut out,
        )
        .await;

    assert_eq!(out.len(), 1);
    let payload = serde_json::to_value(&out[0]).expect("serialize turn event");
    assert_eq!(payload["event_params"]["status"], json!("failed"));
    assert_eq!(payload["event_params"]["turn_error"], json!("badRequest"));
}

#[tokio::test]
async fn turn_lifecycle_emits_interrupted_turn_event_without_error() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ true,
        /*include_resolved_config*/ true,
        /*include_started*/ true,
        /*include_token_usage*/ false,
    )
    .await;
    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Interrupted,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;

    assert_eq!(out.len(), 1);
    let payload = serde_json::to_value(&out[0]).expect("serialize turn event");
    assert_eq!(payload["event_params"]["status"], json!("interrupted"));
    assert_eq!(payload["event_params"]["turn_error"], json!(null));
}

#[tokio::test]
async fn turn_completed_without_started_notification_emits_null_started_at() {
    let mut reducer = AnalyticsReducer::default();
    let mut out = Vec::new();

    ingest_turn_prerequisites(
        &mut reducer,
        &mut out,
        /*include_initialize*/ true,
        /*include_resolved_config*/ true,
        /*include_started*/ false,
        /*include_token_usage*/ false,
    )
    .await;
    reducer
        .ingest(
            AnalyticsFact::Notification(Box::new(sample_turn_completed_notification(
                "thread-2",
                "turn-2",
                AppServerTurnStatus::Completed,
                /*codex_error_info*/ None,
            ))),
            &mut out,
        )
        .await;

    let payload = serde_json::to_value(&out[0]).expect("serialize turn event");
    assert_eq!(payload["event_params"]["started_at"], json!(null));
    assert_eq!(payload["event_params"]["duration_ms"], json!(1234));
    assert_eq!(payload["event_params"]["input_tokens"], json!(null));
    assert_eq!(payload["event_params"]["cached_input_tokens"], json!(null));
    assert_eq!(payload["event_params"]["output_tokens"], json!(null));
    assert_eq!(
        payload["event_params"]["reasoning_output_tokens"],
        json!(null)
    );
    assert_eq!(payload["event_params"]["total_tokens"], json!(null));
}

fn sample_plugin_metadata() -> PluginTelemetryMetadata {
    PluginTelemetryMetadata {
        plugin_id: PluginId::parse("sample@test").expect("valid plugin id"),
        remote_plugin_id: None,
        capability_summary: Some(PluginCapabilitySummary {
            config_name: "sample@test".to_string(),
            display_name: "sample".to_string(),
            description: None,
            has_skills: true,
            mcp_server_names: vec!["mcp-1".to_string(), "mcp-2".to_string()],
            app_connector_ids: vec![
                AppConnectorId("calendar".to_string()),
                AppConnectorId("drive".to_string()),
            ],
        }),
    }
}

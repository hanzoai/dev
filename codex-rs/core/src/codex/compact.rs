use std::sync::Arc;

use super::get_last_assistant_message_from_turn;
use super::AgentTask;
use super::MutexExt;
use super::Session;
use super::TurnContext;
use crate::error::Result as CodexResult;
use crate::protocol::AgentMessageEvent;
use crate::protocol::EventMsg;
use crate::protocol::InputItem;
use crate::protocol::TaskCompleteEvent;
use askama::Template;
use codex_protocol::models::ContentItem;
use codex_protocol::models::ResponseItem;
use codex_protocol::protocol::CompactedItem;
use codex_protocol::protocol::InputMessageKind;
use codex_protocol::protocol::RolloutItem;
use codex_protocol::protocol::TurnContextItem;
use crate::config_types::ReasoningEffort as ReasoningEffortConfig;
use crate::config_types::ReasoningSummary as ReasoningSummaryConfig;
use crate::protocol::AskForApproval as AskForApprovalCore;
use crate::protocol::SandboxPolicy as SandboxPolicyCore;

pub(super) const COMPACT_TRIGGER_TEXT: &str = "Start Summarization";
const SUMMARIZATION_PROMPT: &str = include_str!("../../templates/compact/prompt.md");

#[derive(Template)]
#[template(path = "compact/history_bridge.md", escape = "none")]
struct HistoryBridgeTemplate<'a> {
    user_messages_text: &'a str,
    summary_text: &'a str,
}

pub(super) fn spawn_compact_task(
    sess: Arc<Session>,
    turn_context: Arc<TurnContext>,
    sub_id: String,
    input: Vec<InputItem>,
) {
    let task = AgentTask::compact(
        sess.clone(),
        turn_context,
        sub_id,
        input,
        SUMMARIZATION_PROMPT.to_string(),
    );
    sess.set_task(task);
}

pub(super) async fn run_inline_auto_compact_task(
    sess: Arc<Session>,
    turn_context: Arc<TurnContext>,
) -> Vec<ResponseItem> {
    let sub_id = sess.next_internal_sub_id();
    let input = vec![InputItem::Text { text: COMPACT_TRIGGER_TEXT.to_string() }];
    match perform_compaction(
        Arc::clone(&sess),
        Arc::clone(&turn_context),
        sub_id,
        input,
        SUMMARIZATION_PROMPT.to_string(),
        false,
    )
    .await
    {
        Ok(history) => history,
        Err(_) => Vec::new(),
    }
}

#[allow(dead_code)]
pub(super) async fn run_compact_task(
    sess: Arc<Session>,
    turn_context: Arc<TurnContext>,
    sub_id: String,
    input: Vec<InputItem>,
    compact_instructions: String,
) {
    let _ = perform_compaction(sess, turn_context, sub_id, input, compact_instructions, true).await;
}

/// Perform a compact operation and return the rebuilt conversation history.
///
/// This minimal implementation avoids invoking the model and instead composes
/// a compacted history using the last assistant message as the summary.
pub(super) async fn perform_compaction(
    sess: Arc<Session>,
    turn_context: Arc<TurnContext>,
    sub_id: String,
    _input: Vec<InputItem>,
    _compact_instructions: String,
    remove_task_on_completion: bool,
) -> CodexResult<Vec<ResponseItem>> {
    // Signal task begin
    let start_event = sess.make_event(&sub_id, EventMsg::TaskStarted);
    sess.send_event(start_event).await;

    // Snapshot current history
    let history_snapshot = {
        let state = sess.state.lock_unchecked();
        state.history.contents()
    };

    // Build compacted history using the last assistant message as the summary
    let summary_text = get_last_assistant_message_from_turn(&history_snapshot).unwrap_or_default();
    let user_messages = collect_user_messages(&history_snapshot);
    let initial_context = sess.build_initial_context(turn_context.as_ref());
    let new_history = build_compacted_history(initial_context, &user_messages, &summary_text);

    // Persist rollout items for traceability
    let ctx_item = RolloutItem::TurnContext(TurnContextItem {
        cwd: turn_context.cwd.clone(),
        approval_policy: map_approval(turn_context.approval_policy),
        sandbox_policy: map_sandbox(&turn_context.sandbox_policy),
        model: turn_context.client.get_model(),
        effort: Some(map_effort(turn_context.client.get_reasoning_effort())),
        summary: map_summary(turn_context.client.get_reasoning_summary()),
    });
    let compact_item = RolloutItem::Compacted(CompactedItem { message: summary_text.clone() });
    sess.persist_rollout_items(&[ctx_item, compact_item]).await;

    if remove_task_on_completion {
        sess.remove_task(&sub_id);
    }

    // Notify completion
    let done_msg = sess.make_event(&sub_id, EventMsg::AgentMessage(AgentMessageEvent {
        message: "Compact task completed".to_string(),
    }));
    sess.send_event(done_msg).await;
    let complete = sess.make_event(
        &sub_id,
        EventMsg::TaskComplete(TaskCompleteEvent { last_agent_message: None }),
    );
    sess.send_event(complete).await;

    Ok(new_history)
}

fn map_approval(a: AskForApprovalCore) -> codex_protocol::protocol::AskForApproval {
    match a {
        AskForApprovalCore::UnlessTrusted => codex_protocol::protocol::AskForApproval::UnlessTrusted,
        AskForApprovalCore::OnFailure => codex_protocol::protocol::AskForApproval::OnFailure,
        AskForApprovalCore::OnRequest => codex_protocol::protocol::AskForApproval::OnRequest,
        AskForApprovalCore::Never => codex_protocol::protocol::AskForApproval::Never,
    }
}

fn map_sandbox(s: &SandboxPolicyCore) -> codex_protocol::protocol::SandboxPolicy {
    match s {
        SandboxPolicyCore::DangerFullAccess => codex_protocol::protocol::SandboxPolicy::DangerFullAccess,
        SandboxPolicyCore::ReadOnly => codex_protocol::protocol::SandboxPolicy::ReadOnly,
        SandboxPolicyCore::WorkspaceWrite {
            writable_roots,
            network_access,
            exclude_tmpdir_env_var,
            exclude_slash_tmp,
            ..
        } => codex_protocol::protocol::SandboxPolicy::WorkspaceWrite {
            writable_roots: writable_roots.clone(),
            network_access: *network_access,
            exclude_tmpdir_env_var: *exclude_tmpdir_env_var,
            exclude_slash_tmp: *exclude_slash_tmp,
        },
    }
}

fn map_effort(e: ReasoningEffortConfig) -> codex_protocol::config_types::ReasoningEffort {
    match e {
        ReasoningEffortConfig::Minimal => codex_protocol::config_types::ReasoningEffort::Minimal,
        ReasoningEffortConfig::Low => codex_protocol::config_types::ReasoningEffort::Low,
        ReasoningEffortConfig::Medium => codex_protocol::config_types::ReasoningEffort::Medium,
        ReasoningEffortConfig::High => codex_protocol::config_types::ReasoningEffort::High,
        ReasoningEffortConfig::None => codex_protocol::config_types::ReasoningEffort::Minimal,
    }
}

fn map_summary(s: ReasoningSummaryConfig) -> codex_protocol::config_types::ReasoningSummary {
    match s {
        ReasoningSummaryConfig::Auto => codex_protocol::config_types::ReasoningSummary::Auto,
        ReasoningSummaryConfig::Concise => codex_protocol::config_types::ReasoningSummary::Concise,
        ReasoningSummaryConfig::Detailed => codex_protocol::config_types::ReasoningSummary::Detailed,
        ReasoningSummaryConfig::None => codex_protocol::config_types::ReasoningSummary::None,
    }
}

pub fn content_items_to_text(content: &[ContentItem]) -> Option<String> {
    let mut pieces = Vec::new();
    for item in content {
        match item {
            ContentItem::InputText { text } | ContentItem::OutputText { text } => {
                if !text.is_empty() {
                    pieces.push(text.as_str());
                }
            }
            ContentItem::InputImage { .. } => {}
        }
    }
    if pieces.is_empty() {
        None
    } else {
        Some(pieces.join("\n"))
    }
}

pub(crate) fn collect_user_messages(items: &[ResponseItem]) -> Vec<String> {
    items
        .iter()
        .filter_map(|item| match item {
            ResponseItem::Message { role, content, .. } if role == "user" => {
                content_items_to_text(content)
            }
            _ => None,
        })
        .filter(|text| !is_session_prefix_message(text))
        .collect()
}

pub fn is_session_prefix_message(text: &str) -> bool {
    matches!(
        InputMessageKind::from(("user", text)),
        InputMessageKind::UserInstructions | InputMessageKind::EnvironmentContext
    )
}

pub(crate) fn build_compacted_history(
    initial_context: Vec<ResponseItem>,
    user_messages: &[String],
    summary_text: &str,
) -> Vec<ResponseItem> {
    let mut history = initial_context;
    let user_messages_text = if user_messages.is_empty() {
        "(none)".to_string()
    } else {
        user_messages.join("\n\n")
    };
    let summary_text = if summary_text.is_empty() {
        "(no summary available)".to_string()
    } else {
        summary_text.to_string()
    };
    let Ok(bridge) = HistoryBridgeTemplate {
        user_messages_text: &user_messages_text,
        summary_text: &summary_text,
    }
    .render() else {
        return vec![];
    };
    history.push(ResponseItem::Message {
        id: None,
        role: "user".to_string(),
        content: vec![ContentItem::InputText { text: bridge }],
    });
    history
}

#[allow(dead_code)]
async fn drain_to_completed() -> CodexResult<()> {
    // Legacy streaming path is replaced by minimal compaction above.
    // Keeping a stub to preserve upstream symbol without build warnings.
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn content_items_to_text_joins_non_empty_segments() {
        let items = vec![
            ContentItem::InputText {
                text: "hello".to_string(),
            },
            ContentItem::OutputText {
                text: String::new(),
            },
            ContentItem::OutputText {
                text: "world".to_string(),
            },
        ];

        let joined = content_items_to_text(&items);

        assert_eq!(Some("hello\nworld".to_string()), joined);
    }

    #[test]
    fn content_items_to_text_ignores_image_only_content() {
        let items = vec![ContentItem::InputImage {
            image_url: "file://image.png".to_string(),
        }];

        let joined = content_items_to_text(&items);

        assert_eq!(None, joined);
    }

    #[test]
    fn collect_user_messages_extracts_user_text_only() {
        let items = vec![
            ResponseItem::Message {
                id: Some("assistant".to_string()),
                role: "assistant".to_string(),
                content: vec![ContentItem::OutputText {
                    text: "ignored".to_string(),
                }],
            },
            ResponseItem::Message {
                id: Some("user".to_string()),
                role: "user".to_string(),
                content: vec![
                    ContentItem::InputText {
                        text: "first".to_string(),
                    },
                    ContentItem::OutputText {
                        text: "second".to_string(),
                    },
                ],
            },
            ResponseItem::Other,
        ];

        let collected = collect_user_messages(&items);

        assert_eq!(vec!["first\nsecond".to_string()], collected);
    }

    #[test]
    fn collect_user_messages_filters_session_prefix_entries() {
        let items = vec![
            ResponseItem::Message {
                id: None,
                role: "user".to_string(),
                content: vec![ContentItem::InputText {
                    text: "<user_instructions>do things</user_instructions>".to_string(),
                }],
            },
            ResponseItem::Message {
                id: None,
                role: "user".to_string(),
                content: vec![ContentItem::InputText {
                    text: "<ENVIRONMENT_CONTEXT>cwd=/tmp</ENVIRONMENT_CONTEXT>".to_string(),
                }],
            },
            ResponseItem::Message {
                id: None,
                role: "user".to_string(),
                content: vec![ContentItem::InputText {
                    text: "real user message".to_string(),
                }],
            },
        ];

        let collected = collect_user_messages(&items);

        assert_eq!(vec!["real user message".to_string()], collected);
    }
}

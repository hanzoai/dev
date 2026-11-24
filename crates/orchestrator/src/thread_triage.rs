//! `ThreadTriageDecider.ts`: pure thread organization and triage command decisions.

use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadTriageState {
    pub thread_id: String,
    pub archived: bool,
    pub pinned_at: Option<String>,
    pub pin_order_key: Option<String>,
    pub active_order_key: Option<String>,
    pub settled_override: Option<String>,
    pub settled_at: Option<String>,
    pub unsettled_at: Option<String>,
    pub snoozed_until: Option<String>,
    pub snoozed_at: Option<String>,
    pub last_snooze_wake_at: Option<String>,
    pub auto_settle_disabled_at: Option<String>,
    pub tags: Vec<String>,
    pub last_visited_at: Option<String>,
    pub updated_at: String,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(tag = "type")]
pub enum ThreadTriageCommand {
    #[serde(rename = "thread.pin", rename_all = "camelCase")]
    Pin { order_key: Option<String> },
    #[serde(rename = "thread.unpin")]
    Unpin,
    #[serde(rename = "thread.pin.reorder", rename_all = "camelCase")]
    PinReorder { order_key: String },
    #[serde(rename = "thread.active.reorder", rename_all = "camelCase")]
    ActiveReorder { order_key: String },
    #[serde(rename = "thread.tag.set", rename_all = "camelCase")]
    TagSet { tag: String, tagged: bool },
    #[serde(rename = "thread.unsnooze")]
    Unsnooze,
    #[serde(rename = "thread.auto-settle.set", rename_all = "camelCase")]
    AutoSettleSet { enabled: bool },
    #[serde(rename = "thread.mark-unread", rename_all = "camelCase")]
    MarkUnread { mark_unread_visited_at: Option<String> },
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadTriageInput {
    pub state: ThreadTriageState,
    pub command: ThreadTriageCommand,
    pub now_iso: String,
    pub now_ms: i64,
    pub snoozed_until_ms: Option<i64>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadTriagePatch {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pinned_at: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pin_order_key: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub active_order_key: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub settled_override: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub settled_at: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unsettled_at: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub snoozed_until: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub snoozed_at: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub last_snooze_wake_at: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auto_settle_disabled_at: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub last_visited_at: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub updated_at: Option<String>,
}

impl ThreadTriagePatch {
    fn empty() -> Self {
        Self {
            pinned_at: None,
            pin_order_key: None,
            active_order_key: None,
            settled_override: None,
            settled_at: None,
            unsettled_at: None,
            snoozed_until: None,
            snoozed_at: None,
            last_snooze_wake_at: None,
            auto_settle_disabled_at: None,
            tags: None,
            last_visited_at: None,
            updated_at: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(untagged)]
pub enum ThreadTriageDecision {
    Err {
        ok: bool,
        error: String,
    },
    Ok {
        ok: bool,
        patch: ThreadTriagePatch,
    },
}

pub fn decide_thread_triage(input: &ThreadTriageInput) -> ThreadTriageDecision {
    let state = &input.state;
    let now = &input.now_iso;

    let rejects_archived = matches!(
        input.command,
        ThreadTriageCommand::Pin { .. }
            | ThreadTriageCommand::Unpin
            | ThreadTriageCommand::PinReorder { .. }
            | ThreadTriageCommand::ActiveReorder { .. }
            | ThreadTriageCommand::Unsnooze
            | ThreadTriageCommand::AutoSettleSet { .. }
    );
    if rejects_archived && state.archived {
        return ThreadTriageDecision::Err {
            ok: false,
            error: format!("Thread {} is archived.", state.thread_id),
        };
    }

    match &input.command {
        ThreadTriageCommand::Pin { order_key } => {
            let already_pinned = state.pinned_at.is_some();
            let is_settled = state.settled_override.as_deref() == Some("settled");
            let promotes = is_settled || state.snoozed_until.is_some();
            let mut patch = ThreadTriagePatch::empty();
            patch.pinned_at = Some(if already_pinned {
                state.pinned_at.clone()
            } else {
                Some(now.clone())
            });
            if !already_pinned
                && let Some(key) = order_key
            {
                patch.pin_order_key = Some(Some(key.clone()));
            }
            patch.settled_override = Some(if is_settled {
                Some("active".to_string())
            } else {
                state.settled_override.clone()
            });
            patch.settled_at = Some(if is_settled {
                None
            } else {
                state.settled_at.clone()
            });
            if is_settled {
                patch.unsettled_at = Some(Some(now.clone()));
            }
            patch.snoozed_until = Some(None);
            patch.snoozed_at = Some(None);
            patch.last_snooze_wake_at = Some(if state.snoozed_until.is_none() {
                state.last_snooze_wake_at.clone()
            } else {
                Some(now.clone())
            });
            patch.updated_at = Some(if already_pinned && !promotes {
                state.updated_at.clone()
            } else {
                now.clone()
            });
            ThreadTriageDecision::Ok { ok: true, patch }
        }
        ThreadTriageCommand::Unpin => {
            let already_unpinned = state.pinned_at.is_none();
            let mut patch = ThreadTriagePatch::empty();
            patch.pinned_at = Some(None);
            patch.pin_order_key = Some(None);
            patch.updated_at = Some(if already_unpinned {
                state.updated_at.clone()
            } else {
                now.clone()
            });
            ThreadTriageDecision::Ok { ok: true, patch }
        }
        ThreadTriageCommand::PinReorder { order_key } => {
            if state.pinned_at.is_none() {
                return ThreadTriageDecision::Err {
                    ok: false,
                    error: format!(
                        "Thread {} is not pinned and cannot be reordered.",
                        state.thread_id
                    ),
                };
            }
            let key_unchanged = state.pin_order_key.as_deref() == Some(order_key.as_str());
            let mut patch = ThreadTriagePatch::empty();
            patch.pin_order_key = Some(Some(order_key.clone()));
            patch.updated_at = Some(if key_unchanged {
                state.updated_at.clone()
            } else {
                now.clone()
            });
            ThreadTriageDecision::Ok { ok: true, patch }
        }
        ThreadTriageCommand::ActiveReorder { order_key } => {
            if state.pinned_at.is_some() || state.settled_override.as_deref() == Some("settled") {
                return ThreadTriageDecision::Err {
                    ok: false,
                    error: format!(
                        "Thread {} is not active and cannot be reordered.",
                        state.thread_id
                    ),
                };
            }
            let mut patch = ThreadTriagePatch::empty();
            patch.active_order_key = Some(Some(order_key.clone()));
            patch.updated_at = Some(state.updated_at.clone());
            ThreadTriageDecision::Ok { ok: true, patch }
        }
        ThreadTriageCommand::TagSet { tag, tagged } => {
            let tags = if *tagged {
                if state.tags.contains(tag) {
                    state.tags.clone()
                } else {
                    let mut next = state.tags.clone();
                    next.push(tag.clone());
                    next
                }
            } else {
                state.tags.iter().filter(|t| *t != tag).cloned().collect()
            };
            let mut patch = ThreadTriagePatch::empty();
            patch.tags = Some(tags);
            ThreadTriageDecision::Ok { ok: true, patch }
        }
        ThreadTriageCommand::Unsnooze => {
            let already_awake = state.snoozed_until.is_none();
            let mut patch = ThreadTriagePatch::empty();
            patch.snoozed_until = Some(None);
            patch.snoozed_at = Some(None);
            patch.last_snooze_wake_at = Some(if already_awake {
                state.last_snooze_wake_at.clone()
            } else {
                Some(now.clone())
            });
            patch.updated_at = Some(if already_awake {
                state.updated_at.clone()
            } else {
                now.clone()
            });
            ThreadTriageDecision::Ok { ok: true, patch }
        }
        ThreadTriageCommand::AutoSettleSet { enabled } => {
            let unchanged = *enabled == state.auto_settle_disabled_at.is_none();
            let mut patch = ThreadTriagePatch::empty();
            patch.auto_settle_disabled_at = Some(if *enabled {
                None
            } else {
                state
                    .auto_settle_disabled_at
                    .clone()
                    .or_else(|| Some(now.clone()))
            });
            patch.updated_at = Some(if unchanged {
                state.updated_at.clone()
            } else {
                now.clone()
            });
            ThreadTriageDecision::Ok { ok: true, patch }
        }
        ThreadTriageCommand::MarkUnread {
            mark_unread_visited_at,
        } => {
            let snooze_expired = state.snoozed_until.is_some()
                && input.snoozed_until_ms.is_some_and(|ms| ms <= input.now_ms);
            let mut patch = ThreadTriagePatch::empty();
            patch.last_visited_at = Some(mark_unread_visited_at.clone());
            if snooze_expired {
                patch.snoozed_until = Some(None);
                patch.snoozed_at = Some(None);
                patch.last_snooze_wake_at = Some(state.snoozed_until.clone());
            }
            ThreadTriageDecision::Ok { ok: true, patch }
        }
    }
}

pub fn apply(name: &str, input: &RawValue) -> Option<Result<String, serde_json::Error>> {
    if name != "threadTriageCommand" {
        return None;
    }
    Some(
        serde_json::from_str::<ThreadTriageInput>(input.get())
            .and_then(|v| serde_json::to_string(&decide_thread_triage(&v))),
    )
}

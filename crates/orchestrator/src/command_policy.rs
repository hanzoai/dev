//! `CommandPolicy.ts`: pure dispatch-intent and capability-policy decisions.

use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;

/// JS `String.prototype.trim` whitespace, which is not Rust's.
fn js_trim(s: &str) -> &str {
    s.trim_matches(|c: char| matches!(c, '\u{FEFF}') || (c.is_whitespace() && c != '\u{85}'))
}

fn is_active_run_status(status: &str) -> bool {
    matches!(status, "preparing" | "starting" | "running" | "waiting")
}

fn is_native_maintenance_str(text: &str, attachments: usize) -> bool {
    if attachments != 0 {
        return false;
    }
    let normalized = js_trim(text).to_lowercase();
    normalized == "/compact" || normalized == "/logout"
}

fn provider_switch_waits_for_turn_flags(
    supports_queued_messages: bool,
    supports_interrupt: bool,
    supports_steering_by_interrupt_restart: bool,
) -> bool {
    supports_queued_messages && !(supports_interrupt && supports_steering_by_interrupt_restart)
}

fn can_fork_busy_source_natively_flags(
    can_fork_while_source_running: bool,
    exposes_native_turn_id: bool,
    has_native_turn_boundary: bool,
) -> bool {
    can_fork_while_source_running && (has_native_turn_boundary || !exposes_native_turn_id)
}

#[derive(Serialize)]
struct CheckOk<T: Serialize> {
    ok: bool,
    value: T,
}

#[derive(Serialize)]
struct CheckErr {
    ok: bool,
    capability: &'static str,
    detail: &'static str,
}

fn ok_null() -> Result<String, serde_json::Error> {
    serde_json::to_string(&CheckOk::<Option<()>> {
        ok: true,
        value: None,
    })
}

fn ok_val(value: &'static str) -> Result<String, serde_json::Error> {
    serde_json::to_string(&CheckOk { ok: true, value })
}

fn err_cap(capability: &'static str, detail: &'static str) -> Result<String, serde_json::Error> {
    serde_json::to_string(&CheckErr {
        ok: false,
        capability,
        detail,
    })
}

#[derive(Deserialize)]
struct NativeMaintenanceInput {
    text: String,
    attachments: usize,
}

#[derive(Deserialize)]
struct GoalStopsInput {
    #[serde(rename = "commandType")]
    command_type: String,
    #[serde(rename = "goalStatus")]
    goal_status: Option<String>,
}

#[derive(Deserialize)]
struct CanForkBusyInput {
    #[serde(rename = "canForkWhileSourceRunning")]
    can_fork_while_source_running: bool,
    #[serde(rename = "exposesNativeTurnId")]
    exposes_native_turn_id: bool,
    #[serde(rename = "hasNativeTurnBoundary")]
    has_native_turn_boundary: bool,
}

#[derive(Deserialize)]
struct TurnCaps {
    #[serde(rename = "supportsQueuedMessages", default)]
    supports_queued_messages: bool,
    #[serde(rename = "supportsActiveSteering", default)]
    supports_active_steering: bool,
    #[serde(rename = "supportsInterrupt", default)]
    supports_interrupt: bool,
    #[serde(rename = "supportsSteeringByInterruptRestart", default)]
    supports_steering_by_interrupt_restart: bool,
}

#[derive(Deserialize)]
struct SteeringInput {
    #[serde(rename = "forceRestart", default)]
    force_restart: bool,
    #[serde(rename = "supportsActiveSteering")]
    supports_active_steering: bool,
    #[serde(rename = "supportsInterrupt")]
    supports_interrupt: bool,
    #[serde(rename = "supportsSteeringByInterruptRestart")]
    supports_steering_by_interrupt_restart: bool,
}

#[derive(Deserialize)]
struct EnsureQueuedInput {
    #[serde(rename = "supportsQueuedMessages")]
    supports_queued_messages: bool,
}

#[derive(Deserialize)]
struct EnsureInterruptInput {
    #[serde(rename = "supportsInterrupt")]
    supports_interrupt: bool,
}

#[derive(Deserialize)]
struct EnsureNativeForkInput {
    #[serde(rename = "canForkThread")]
    can_fork_thread: bool,
    #[serde(rename = "canForkFromTurn")]
    can_fork_from_turn: bool,
    #[serde(rename = "fromSpecificTurn")]
    from_specific_turn: bool,
    #[serde(rename = "nativeThreadIds")]
    native_thread_ids: String,
}

#[derive(Deserialize)]
struct EnsureRollbackInput {
    #[serde(rename = "canRollbackThread")]
    can_rollback_thread: bool,
    #[serde(rename = "providerCanRollbackConversation")]
    provider_can_rollback_conversation: bool,
    #[serde(rename = "canConsumeHandoffSummaries")]
    can_consume_handoff_summaries: bool,
    #[serde(rename = "providerRollbackReturnsSnapshot")]
    provider_rollback_returns_snapshot: bool,
}

#[derive(Deserialize)]
struct EnsureContextHandoffInput {
    strategy: String,
    #[serde(rename = "canConsumeHandoffSummaries")]
    can_consume_handoff_summaries: bool,
    #[serde(rename = "acceptsSyntheticUserContext")]
    accepts_synthetic_user_context: bool,
    #[serde(rename = "supportsDeltaHandoff", default)]
    supports_delta_handoff: bool,
    #[serde(rename = "supportsFullThreadHandoff", default)]
    supports_full_thread_handoff: bool,
}

#[derive(Deserialize)]
struct ForkExecutionInput {
    #[serde(rename = "sameProvider")]
    same_provider: bool,
    #[serde(rename = "hasStrongNativeSource")]
    has_strong_native_source: bool,
    #[serde(rename = "hasNativeTurnBoundary")]
    has_native_turn_boundary: bool,
    #[serde(rename = "sourceRunStatus")]
    source_run_status: String,
    #[serde(rename = "fromSpecificTurn")]
    from_specific_turn: bool,
    #[serde(rename = "sourceBusy")]
    source_busy: bool,
    #[serde(rename = "canForkThread")]
    can_fork_thread: bool,
    #[serde(rename = "canForkFromTurn")]
    can_fork_from_turn: bool,
    #[serde(rename = "canForkWhileSourceRunning")]
    can_fork_while_source_running: bool,
    #[serde(rename = "exposesNativeTurnId")]
    exposes_native_turn_id: bool,
    #[serde(rename = "nativeThreadIds")]
    native_thread_ids: String,
    #[serde(rename = "canConsumeHandoffSummaries")]
    can_consume_handoff_summaries: bool,
    #[serde(rename = "acceptsSyntheticUserContext")]
    accepts_synthetic_user_context: bool,
    #[serde(rename = "supportsFullThreadHandoff")]
    supports_full_thread_handoff: bool,
}

#[derive(Deserialize)]
struct DispatchRun {
    id: String,
    status: String,
    #[serde(rename = "userMessageId")]
    user_message_id: String,
    #[serde(rename = "hasRunningProviderTurn")]
    has_running_provider_turn: bool,
    turns: Option<TurnCaps>,
}

#[derive(Deserialize)]
struct DispatchMessage {
    id: String,
    text: String,
    attachments: usize,
}

#[derive(Clone, Deserialize, Serialize)]
struct DispatchMode {
    #[serde(rename = "type")]
    kind: String,
    #[serde(rename = "targetRunId", skip_serializing_if = "Option::is_none")]
    target_run_id: Option<String>,
}

#[derive(Deserialize)]
struct DispatchIntentInput {
    runs: Vec<DispatchRun>,
    messages: Vec<DispatchMessage>,
    #[serde(rename = "requestedMode")]
    requested_mode: DispatchMode,
    #[serde(rename = "deliveryIntent")]
    delivery_intent: Option<String>,
}

fn ensure_context_handoff_flags(
    strategy: &str,
    can_consume_handoff_summaries: bool,
    accepts_synthetic_user_context: bool,
    supports_delta_handoff: bool,
    supports_full_thread_handoff: bool,
) -> Option<(&'static str, &'static str)> {
    if !can_consume_handoff_summaries {
        return Some((
            "context_handoff",
            "providerInstanceId cannot consume handoff summaries",
        ));
    }
    if !accepts_synthetic_user_context {
        return Some((
            "context_handoff",
            "providerInstanceId cannot receive synthetic user context",
        ));
    }
    if (strategy == "fork_delta_context" || strategy == "delta_context") && !supports_delta_handoff
    {
        return Some((
            "context_handoff",
            "providerInstanceId does not support delta handoff",
        ));
    }
    if strategy == "full_thread_summary" && !supports_full_thread_handoff {
        return Some((
            "context_handoff",
            "providerInstanceId does not support full-thread handoff",
        ));
    }
    None
}

fn resolve_delivery_intent(input: &DispatchIntentInput) -> DispatchMode {
    let Some(intent) = input.delivery_intent.as_deref() else {
        return input.requested_mode.clone();
    };
    let Some(active_run) = input
        .runs
        .iter()
        .rev()
        .find(|r| is_active_run_status(&r.status))
    else {
        return DispatchMode {
            kind: "start_immediately".to_owned(),
            target_run_id: None,
        };
    };
    if intent == "steer" {
        return DispatchMode {
            kind: "steer_active".to_owned(),
            target_run_id: Some(active_run.id.clone()),
        };
    }
    if intent == "restart" {
        return DispatchMode {
            kind: "restart_active".to_owned(),
            target_run_id: Some(active_run.id.clone()),
        };
    }
    if active_run.status == "preparing"
        || active_run.status == "starting"
        || !active_run.has_running_provider_turn
    {
        return DispatchMode {
            kind: "queue_after_active".to_owned(),
            target_run_id: None,
        };
    }
    if let Some(caps) = &active_run.turns {
        if caps.supports_active_steering {
            return DispatchMode {
                kind: "steer_active".to_owned(),
                target_run_id: Some(active_run.id.clone()),
            };
        }
        if caps.supports_queued_messages {
            return DispatchMode {
                kind: "queue_after_active".to_owned(),
                target_run_id: None,
            };
        }
        if caps.supports_steering_by_interrupt_restart {
            return DispatchMode {
                kind: "restart_active".to_owned(),
                target_run_id: Some(active_run.id.clone()),
            };
        }
    }
    DispatchMode {
        kind: "queue_after_active".to_owned(),
        target_run_id: None,
    }
}

fn resolve_message_dispatch_intent(input: &RawValue) -> Result<String, serde_json::Error> {
    let input: DispatchIntentInput = serde_json::from_str(input.get())?;
    let mode = resolve_delivery_intent(&input);
    if mode.kind != "steer_active" {
        return serde_json::to_string(&mode);
    }
    let target = mode
        .target_run_id
        .as_deref()
        .and_then(|id| input.runs.iter().find(|r| r.id == id));
    let target_message =
        target.and_then(|run| input.messages.iter().find(|m| m.id == run.user_message_id));
    if let Some(msg) = target_message {
        if is_native_maintenance_str(&msg.text, msg.attachments) {
            return serde_json::to_string(&DispatchMode {
                kind: "queue_after_active".to_owned(),
                target_run_id: None,
            });
        }
    }
    let turns = target
        .filter(|run| is_active_run_status(&run.status))
        .and_then(|run| run.turns.as_ref());
    if let Some(turns) = turns {
        if !turns.supports_active_steering
            && provider_switch_waits_for_turn_flags(
                turns.supports_queued_messages,
                turns.supports_interrupt,
                turns.supports_steering_by_interrupt_restart,
            )
        {
            return serde_json::to_string(&DispatchMode {
                kind: "queue_after_active".to_owned(),
                target_run_id: None,
            });
        }
    }
    serde_json::to_string(&mode)
}

pub fn apply(name: &str, input: &RawValue) -> Option<Result<String, serde_json::Error>> {
    match name {
        "commandPolicyNativeMaintenance" => Some(
            serde_json::from_str::<NativeMaintenanceInput>(input.get()).and_then(|i| {
                serde_json::to_string(&is_native_maintenance_str(&i.text, i.attachments))
            }),
        ),
        "commandPolicyGoalStopsActiveRun" => Some(
            serde_json::from_str::<GoalStopsInput>(input.get()).and_then(|i| {
                let stops = match i.command_type.as_str() {
                    "set" | "resume" => true,
                    "clear" | "pause" => i.goal_status.as_deref() == Some("active"),
                    _ => false,
                };
                serde_json::to_string(&stops)
            }),
        ),
        "commandPolicyCanForkBusySourceNatively" => Some(
            serde_json::from_str::<CanForkBusyInput>(input.get()).and_then(|i| {
                serde_json::to_string(&can_fork_busy_source_natively_flags(
                    i.can_fork_while_source_running,
                    i.exposes_native_turn_id,
                    i.has_native_turn_boundary,
                ))
            }),
        ),
        "commandPolicyProviderSwitchWaitsForTurn" => Some(
            serde_json::from_str::<TurnCaps>(input.get()).and_then(|i| {
                serde_json::to_string(&provider_switch_waits_for_turn_flags(
                    i.supports_queued_messages,
                    i.supports_interrupt,
                    i.supports_steering_by_interrupt_restart,
                ))
            }),
        ),
        "commandPolicySteeringExecution" => Some(
            serde_json::from_str::<SteeringInput>(input.get()).and_then(|i| {
                if !i.force_restart && i.supports_active_steering {
                    return ok_val("active_steering");
                }
                if i.supports_interrupt && i.supports_steering_by_interrupt_restart {
                    return ok_val("interrupt_restart");
                }
                err_cap(
                    if i.force_restart || i.supports_interrupt {
                        "interrupt_restart_steering"
                    } else {
                        "active_steering"
                    },
                    if i.force_restart {
                        "providerInstanceId cannot satisfy a required interrupt-and-restart"
                    } else {
                        "providerInstanceId cannot steer active turns directly or by interrupt-and-restart"
                    },
                )
            }),
        ),
        "commandPolicyEnsureQueuedMessages" => Some(
            serde_json::from_str::<EnsureQueuedInput>(input.get()).and_then(|i| {
                if i.supports_queued_messages {
                    ok_null()
                } else {
                    err_cap(
                        "queued_messages",
                        "providerInstanceId does not support app-owned queued turns",
                    )
                }
            }),
        ),
        "commandPolicyEnsureInterrupt" => Some(
            serde_json::from_str::<EnsureInterruptInput>(input.get()).and_then(|i| {
                if i.supports_interrupt {
                    ok_null()
                } else {
                    err_cap("interrupt", "providerInstanceId does not support turn interrupts")
                }
            }),
        ),
        "commandPolicyEnsureNativeFork" => Some(
            serde_json::from_str::<EnsureNativeForkInput>(input.get()).and_then(|i| {
                if !i.can_fork_thread {
                    return err_cap(
                        "native_fork",
                        "providerInstanceId does not support native thread forks",
                    );
                }
                if i.from_specific_turn && !i.can_fork_from_turn {
                    return err_cap(
                        "fork_from_turn",
                        "providerInstanceId cannot fork from a specific completed turn",
                    );
                }
                if i.native_thread_ids != "strong" {
                    return err_cap(
                        "native_fork",
                        "providerInstanceId does not expose strong native thread ids",
                    );
                }
                ok_null()
            }),
        ),
        "commandPolicyEnsureRollback" => Some(
            serde_json::from_str::<EnsureRollbackInput>(input.get()).and_then(|i| {
                if !i.can_rollback_thread
                    || (!i.provider_can_rollback_conversation && !i.can_consume_handoff_summaries)
                {
                    return err_cap(
                        "rollback",
                        "providerInstanceId conversation rollback is unavailable",
                    );
                }
                if !i.provider_rollback_returns_snapshot {
                    return err_cap(
                        "rollback_snapshot",
                        "rollback must return a providerInstanceId thread snapshot",
                    );
                }
                ok_null()
            }),
        ),
        "commandPolicyEnsureContextHandoff" => Some(
            serde_json::from_str::<EnsureContextHandoffInput>(input.get()).and_then(|i| {
                match ensure_context_handoff_flags(
                    &i.strategy,
                    i.can_consume_handoff_summaries,
                    i.accepts_synthetic_user_context,
                    i.supports_delta_handoff,
                    i.supports_full_thread_handoff,
                ) {
                    Some((cap, detail)) => err_cap(cap, detail),
                    None => ok_null(),
                }
            }),
        ),
        "commandPolicyForkExecution" => Some(
            serde_json::from_str::<ForkExecutionInput>(input.get()).and_then(|i| {
                let can_fork_natively = (i.source_run_status == "completed"
                    || i.source_run_status == "waiting")
                    && i.same_provider
                    && i.has_strong_native_source
                    && (!i.source_busy
                        || can_fork_busy_source_natively_flags(
                            i.can_fork_while_source_running,
                            i.exposes_native_turn_id,
                            i.has_native_turn_boundary,
                        ))
                    && i.can_fork_thread
                    && (!i.from_specific_turn || i.can_fork_from_turn)
                    && i.native_thread_ids == "strong";
                if can_fork_natively {
                    return ok_val("native_fork");
                }
                match ensure_context_handoff_flags(
                    "full_thread_summary",
                    i.can_consume_handoff_summaries,
                    i.accepts_synthetic_user_context,
                    false,
                    i.supports_full_thread_handoff,
                ) {
                    Some((cap, detail)) => err_cap(cap, detail),
                    None => ok_val("portable_context"),
                }
            }),
        ),
        "commandPolicyMessageDispatchIntent" => Some(resolve_message_dispatch_intent(input)),
        _ => None,
    }
}

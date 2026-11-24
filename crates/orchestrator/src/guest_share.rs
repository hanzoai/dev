//! `GuestShareDecider.ts`: guest share command, send, cancel, and attachment-release rules.

use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;
use std::collections::BTreeSet;

const GUEST_PENDING_MESSAGE_LIMIT: usize = 3;
const GUEST_RESOLVED_MESSAGE_LIMIT: usize = 20;
const GUEST_PENDING_ATTACHMENT_BYTES_LIMIT: u64 = 20 * 1024 * 1024;

const FAILURE_INACTIVE: &str = "This guest link is no longer active.";
const FAILURE_USED: &str = "This guest link was already used.";
const FAILURE_BUSY: &str = "Wait for the owner to answer your earlier messages.";
const FAILURE_FILES_BUSY: &str =
    "Wait for the owner to answer your earlier messages before you attach more files.";

#[derive(Deserialize)]
struct AttachmentMeta {
    id: String,
    #[serde(rename = "sizeBytes", default)]
    size_bytes: u64,
}

#[derive(Clone, Deserialize, Serialize)]
struct GuestMessage {
    id: String,
    text: String,
    status: String,
    #[serde(rename = "submittedAt")]
    submitted_at: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    attachments: Option<Vec<Box<RawValue>>>,
}

#[derive(Clone, Deserialize, Serialize)]
struct GuestShare {
    id: String,
    host: String,
    #[serde(rename = "createdAt")]
    created_at: String,
    #[serde(rename = "expiresAt")]
    expires_at: String,
    #[serde(rename = "guestName")]
    guest_name: Option<String>,
    messages: Vec<GuestMessage>,
}

fn parse_attachment(raw: &RawValue) -> Result<AttachmentMeta, serde_json::Error> {
    serde_json::from_str(raw.get())
}

fn pending_attachments(
    share: Option<&GuestShare>,
) -> Result<Vec<AttachmentMeta>, serde_json::Error> {
    let mut out = Vec::new();
    if let Some(share) = share {
        for message in &share.messages {
            if message.status == "pending" {
                if let Some(attachments) = &message.attachments {
                    for raw in attachments {
                        out.push(parse_attachment(raw)?);
                    }
                }
            }
        }
    }
    Ok(out)
}

fn trim_resolved(messages: Vec<GuestMessage>) -> Vec<GuestMessage> {
    let mut resolved_budget = GUEST_RESOLVED_MESSAGE_LIMIT;
    let mut kept = Vec::with_capacity(messages.len());
    for message in messages.into_iter().rev() {
        if message.status != "pending" {
            if resolved_budget == 0 {
                continue;
            }
            resolved_budget -= 1;
        }
        kept.push(message);
    }
    kept.reverse();
    kept
}

fn resolve_message(
    share: &GuestShare,
    guest_message_id: &str,
    status: &str,
) -> Result<GuestShare, String> {
    let found = share
        .messages
        .iter()
        .find(|candidate| candidate.id == guest_message_id);
    match found {
        Some(message) if message.status == "pending" => {
            let updated: Vec<GuestMessage> = share
                .messages
                .iter()
                .map(|candidate| {
                    if candidate.id == guest_message_id {
                        let mut next = candidate.clone();
                        next.status = status.to_owned();
                        next
                    } else {
                        candidate.clone()
                    }
                })
                .collect();
            let mut next_share = share.clone();
            next_share.messages = trim_resolved(updated);
            Ok(next_share)
        }
        _ => Err(format!("Guest message {guest_message_id} is not pending.")),
    }
}

#[derive(Deserialize)]
struct ReleasedAttachmentsInput {
    before: Option<GuestShare>,
    after: Option<GuestShare>,
}

fn released_attachments(input: &RawValue) -> Result<String, serde_json::Error> {
    let input: ReleasedAttachmentsInput = serde_json::from_str(input.get())?;
    let mut kept_ids = BTreeSet::new();
    if let (Some(before), Some(after)) = (&input.before, &input.after) {
        if after.id == before.id {
            for message in &after.messages {
                if message.status == "dismissed" {
                    continue;
                }
                if let Some(attachments) = &message.attachments {
                    for raw in attachments {
                        kept_ids.insert(parse_attachment(raw)?.id);
                    }
                }
            }
        }
    }
    let released: Vec<String> = pending_attachments(input.before.as_ref())?
        .into_iter()
        .map(|a| a.id)
        .filter(|id| !kept_ids.contains(id))
        .collect();
    serde_json::to_string(&released)
}

#[derive(Deserialize)]
struct Command {
    #[serde(rename = "type")]
    kind: String,
    #[serde(rename = "shareId")]
    share_id: Option<String>,
    host: Option<String>,
    #[serde(rename = "expiresAt")]
    expires_at: Option<String>,
    #[serde(rename = "guestName")]
    guest_name: Option<String>,
    #[serde(rename = "guestMessageId")]
    guest_message_id: Option<String>,
    text: Option<String>,
    attachments: Option<Vec<Box<RawValue>>>,
}

#[derive(Deserialize)]
struct CommandInput {
    share: Option<GuestShare>,
    archived: bool,
    settled: bool,
    command: Command,
    #[serde(rename = "nowIso")]
    now_iso: String,
    #[serde(rename = "nowMs")]
    now_ms: f64,
    #[serde(rename = "expiresAtMs")]
    expires_at_ms: Option<f64>,
}

#[derive(Serialize)]
struct ErrorResult {
    ok: bool,
    error: String,
}

#[derive(Serialize)]
struct UnchangedResult {
    ok: bool,
    changed: bool,
}

#[derive(Serialize)]
struct ChangedResult {
    ok: bool,
    changed: bool,
    #[serde(rename = "guestShare")]
    guest_share: Option<GuestShare>,
    unsettle: bool,
}

fn err_json(error: impl Into<String>) -> Result<String, serde_json::Error> {
    serde_json::to_string(&ErrorResult {
        ok: false,
        error: error.into(),
    })
}

fn decide_command(input: &RawValue) -> Result<String, serde_json::Error> {
    let input: CommandInput = serde_json::from_str(input.get())?;
    let is_expired = input.expires_at_ms.is_some_and(|exp| input.now_ms >= exp);

    let with_share = |guest_share: Option<GuestShare>, unsettle: bool| {
        serde_json::to_string(&ChangedResult {
            ok: true,
            changed: true,
            guest_share,
            unsettle,
        })
    };

    match input.command.kind.as_str() {
        "thread.guest-share.open" => {
            if input.archived {
                return err_json("Archived threads cannot be shared.");
            }
            if input
                .share
                .as_ref()
                .is_some_and(|s| s.messages.iter().any(|m| m.status == "pending"))
            {
                return err_json(
                    "Answer or dismiss waiting guest messages before replacing this link.",
                );
            }
            with_share(
                Some(GuestShare {
                    id: input.command.share_id.unwrap_or_default(),
                    host: input.command.host.unwrap_or_default(),
                    created_at: input.now_iso,
                    expires_at: input.command.expires_at.unwrap_or_default(),
                    guest_name: None,
                    messages: Vec::new(),
                }),
                false,
            )
        }
        "thread.guest-share.close" => {
            if input.share.is_none() {
                serde_json::to_string(&UnchangedResult {
                    ok: true,
                    changed: false,
                })
            } else {
                with_share(None, false)
            }
        }
        "thread.guest-share.redeem" => {
            let share_id = input.command.share_id.unwrap_or_default();
            let guest_name = input.command.guest_name.unwrap_or_default();
            let Some(mut share) = input.share else {
                return err_json(FAILURE_INACTIVE);
            };
            if share.id != share_id || is_expired {
                return err_json(FAILURE_INACTIVE);
            }
            if share.guest_name.is_some() {
                return err_json(FAILURE_USED);
            }
            share.guest_name = Some(guest_name);
            with_share(Some(share), false)
        }
        "thread.guest-message.submit" => {
            let share_id = input.command.share_id.unwrap_or_default();
            let guest_message_id = input.command.guest_message_id.unwrap_or_default();
            let text = input.command.text.unwrap_or_default();
            let attachments = input.command.attachments.unwrap_or_default();
            let Some(mut share) = input.share else {
                return err_json(FAILURE_INACTIVE);
            };
            if share.id != share_id || share.guest_name.is_none() || is_expired {
                return err_json(FAILURE_INACTIVE);
            }
            let pending_count = share
                .messages
                .iter()
                .filter(|m| m.status == "pending")
                .count();
            if pending_count >= GUEST_PENDING_MESSAGE_LIMIT {
                return err_json(FAILURE_BUSY);
            }
            let mut pending_bytes: u64 = pending_attachments(Some(&share))?
                .into_iter()
                .map(|a| a.size_bytes)
                .sum();
            for raw in &attachments {
                pending_bytes = pending_bytes.saturating_add(parse_attachment(raw)?.size_bytes);
            }
            if !attachments.is_empty() && pending_bytes > GUEST_PENDING_ATTACHMENT_BYTES_LIMIT {
                return err_json(FAILURE_FILES_BUSY);
            }
            share.messages.push(GuestMessage {
                id: guest_message_id,
                text,
                status: "pending".to_owned(),
                submitted_at: input.now_iso,
                attachments: if attachments.is_empty() {
                    None
                } else {
                    Some(attachments)
                },
            });
            share.messages = trim_resolved(share.messages);
            with_share(Some(share), input.settled)
        }
        "thread.guest-message.dismiss" => {
            let guest_message_id = input.command.guest_message_id.unwrap_or_default();
            let Some(share) = input.share else {
                return err_json("This thread has no guest link.");
            };
            match resolve_message(&share, &guest_message_id, "dismissed") {
                Ok(next) => with_share(Some(next), false),
                Err(error) => err_json(error),
            }
        }
        other => err_json(format!("Unknown guest share command: {other}")),
    }
}

#[derive(Deserialize)]
struct SendInput {
    share: Option<GuestShare>,
    #[serde(rename = "guestMessageId")]
    guest_message_id: String,
}

#[derive(Serialize)]
struct GuestAttribution {
    #[serde(rename = "guestMessageId")]
    guest_message_id: String,
    name: String,
    text: String,
    #[serde(rename = "shareId")]
    share_id: String,
}

#[derive(Serialize)]
struct SendOkResult {
    ok: bool,
    #[serde(rename = "guestShare")]
    guest_share: GuestShare,
    text: String,
    guest: GuestAttribution,
}

fn decide_send(input: &RawValue) -> Result<String, serde_json::Error> {
    let input: SendInput = serde_json::from_str(input.get())?;
    let Some(share) = input.share else {
        return err_json("This thread has no guest link.");
    };
    let message = share
        .messages
        .iter()
        .find(|candidate| candidate.id == input.guest_message_id);
    let name = share.guest_name.as_deref().unwrap_or("Guest").to_owned();
    let guest_text = message.map(|m| m.text.clone()).unwrap_or_default();
    match resolve_message(&share, &input.guest_message_id, "sent") {
        Ok(next) => {
            let text = format!("Message from guest {name}, sent by the owner:\n\n{guest_text}");
            serde_json::to_string(&SendOkResult {
                ok: true,
                guest_share: next,
                text,
                guest: GuestAttribution {
                    guest_message_id: input.guest_message_id,
                    name,
                    text: guest_text,
                    share_id: share.id,
                },
            })
        }
        Err(error) => err_json(error),
    }
}

#[derive(Deserialize)]
struct CancelInput {
    share: Option<GuestShare>,
    #[serde(rename = "guestMessageId")]
    guest_message_id: String,
    #[serde(rename = "shareId")]
    share_id: String,
}

#[derive(Serialize)]
struct CancelResult {
    changed: bool,
    #[serde(rename = "guestShare")]
    guest_share: Option<GuestShare>,
}

fn after_cancelled_send(input: &RawValue) -> Result<String, serde_json::Error> {
    let input: CancelInput = serde_json::from_str(input.get())?;
    let Some(mut share) = input.share else {
        return serde_json::to_string(&CancelResult {
            changed: false,
            guest_share: None,
        });
    };
    if share.id != input.share_id {
        return serde_json::to_string(&CancelResult {
            changed: false,
            guest_share: Some(share),
        });
    }
    let is_sent = share
        .messages
        .iter()
        .find(|candidate| candidate.id == input.guest_message_id)
        .is_some_and(|m| m.status == "sent");
    if !is_sent {
        return serde_json::to_string(&CancelResult {
            changed: false,
            guest_share: Some(share),
        });
    }
    for candidate in &mut share.messages {
        if candidate.id == input.guest_message_id {
            candidate.status = "dismissed".to_owned();
        }
    }
    serde_json::to_string(&CancelResult {
        changed: true,
        guest_share: Some(share),
    })
}

pub fn apply(name: &str, input: &RawValue) -> Option<Result<String, serde_json::Error>> {
    match name {
        "guestShareReleasedAttachments" => Some(released_attachments(input)),
        "guestShareCommand" => Some(decide_command(input)),
        "guestShareSend" => Some(decide_send(input)),
        "guestShareAfterCancelledSend" => Some(after_cancelled_send(input)),
        _ => None,
    }
}

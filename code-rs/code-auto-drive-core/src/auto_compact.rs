use std::cmp::min;

use anyhow::Result;
use futures::StreamExt;

use code_core::codex::compact::SUMMARIZATION_PROMPT;
use code_core::model_family::{derive_default_model_family, find_family_for_model};
use code_core::{ModelClient, Prompt, ResponseEvent, TextFormat};
use code_protocol::models::{ContentItem, ResponseItem};


const BYTES_PER_TOKEN: usize = 4;
const MAX_TRANSCRIPT_BYTES: usize = 32_000;
const MAX_COMMANDS_IN_SUMMARY: usize = 5;
const MAX_ACTION_LINES: usize = 5;

pub(crate) struct CheckpointSummary {
    pub message: ResponseItem,
    pub text: String,
}

pub(crate) fn compute_slice_bounds(conversation: &[ResponseItem]) -> Option<(usize, usize)> {
    let goal_idx = conversation.iter().position(|item| {
        matches!(item, ResponseItem::Message { role, .. } if role == "user")
    })?;

    if conversation.len() <= goal_idx + 3 {
        return None;
    }

    let after_goal = &conversation[goal_idx + 1..];
    let token_counts: Vec<usize> = after_goal.iter().map(estimate_item_tokens).collect();
    let total_tokens: usize = token_counts.iter().sum();
    let mut midpoint = goal_idx + 1;

    if total_tokens > 0 {
        let target = (total_tokens + 1) / 2;
        let mut running = 0usize;
        for (offset, count) in token_counts.iter().enumerate() {
            running = running.saturating_add(*count);
            if running >= target {
                midpoint = goal_idx + 1 + offset;
                break;
            }
        }
    } else {
        midpoint = goal_idx + 1 + (after_goal.len() + 1) / 2;
    }

    let slice_start = goal_idx + 1;
    let slice_end = advance_to_turn_boundary(conversation, midpoint + 1);

    if slice_end <= slice_start {
        return None;
    }

    Some((slice_start, slice_end))
}

pub(crate) fn apply_compaction(
    conversation: &mut Vec<ResponseItem>,
    bounds: (usize, usize),
    prev_summary_text: Option<&str>,
    summary_message: ResponseItem,
) -> Option<()> {
    let goal_idx = conversation.iter().position(|item| {
        matches!(item, ResponseItem::Message { role, .. } if role == "user")
    })?;

    let (slice_start, slice_end) = bounds;
    if slice_start <= goal_idx || slice_end > conversation.len() {
        return None;
    }

    let mut rebuilt = Vec::with_capacity(conversation.len() - (slice_end - slice_start) + 2);
    rebuilt.extend_from_slice(&conversation[..=goal_idx]);

    if let Some(prev_text) = prev_summary_text.filter(|text| !text.trim().is_empty()) {
        rebuilt.push(make_checkpoint_message(prev_text.to_string()));
    }

    rebuilt.push(summary_message);
    rebuilt.extend_from_slice(&conversation[slice_end..]);
    *conversation = rebuilt;
    Some(())
}

pub(crate) fn build_checkpoint_summary(
    runtime: &tokio::runtime::Runtime,
    client: &ModelClient,
    model_slug: &str,
    items: &[ResponseItem],
    prev_summary: Option<&str>,
) -> CheckpointSummary {
    let summary_text = match summarize_with_model(runtime, client, model_slug, items, prev_summary) {
        Ok(text) if !text.trim().is_empty() => text,
        Ok(_) | Err(_) => deterministic_summary(items, prev_summary),
    };

    let message = make_checkpoint_message(summary_text.clone());
    CheckpointSummary { message, text: summary_text }
}

fn summarize_with_model(
    runtime: &tokio::runtime::Runtime,
    client: &ModelClient,
    model_slug: &str,
    items: &[ResponseItem],
    prev_summary: Option<&str>,
) -> Result<String> {
    let mut prompt = Prompt::default();
    prompt.store = false;
    prompt.text_format = Some(TextFormat {
        r#type: "text".to_string(),
        name: None,
        strict: None,
        schema: None,
    });
    prompt.model_override = Some(model_slug.to_string());
    let family = find_family_for_model(model_slug)
        .unwrap_or_else(|| derive_default_model_family(model_slug));
    prompt.model_family_override = Some(family);

    prompt
        .input
        .push(plain_message("developer", SUMMARIZATION_PROMPT.to_string()));

    let mut user_text = String::new();
    if let Some(prev) = prev_summary.filter(|text| !text.trim().is_empty()) {
        user_text.push_str("Previous checkpoint summary:\n");
        user_text.push_str(prev);
        user_text.push_str("\n\n");
    }
    user_text.push_str("Conversation slice:\n");
    user_text.push_str(&flatten_items(items));

    prompt.input.push(plain_message("user", user_text));

    runtime.block_on(async move {
        let mut stream = client.stream(&prompt).await?;
        let mut collected = String::new();
        let mut response_items = Vec::new();

        while let Some(event) = stream.next().await {
            match event {
                Ok(ResponseEvent::OutputTextDelta { delta, .. }) => {
                    collected.push_str(&delta);
                }
                Ok(ResponseEvent::OutputItemDone { item, .. }) => {
                    response_items.push(item);
                }
                Ok(ResponseEvent::Completed { .. }) => break,
                Ok(_) => {}
                Err(err) => return Err(err.into()),
            }
        }

        if let Some(message) = response_items.into_iter().find_map(|item| match item {
            ResponseItem::Message { role, content, .. } if role == "assistant" => Some(content),
            _ => None,
        }) {
            let mut text = String::new();
            for chunk in message {
                if let ContentItem::OutputText { text: chunk_text } = chunk {
                    text.push_str(&chunk_text);
                }
            }
            if !text.trim().is_empty() {
                return Ok(text);
            }
        }

        Ok(collected)
    })
}

fn deterministic_summary(items: &[ResponseItem], prev_summary: Option<&str>) -> String {
    let mut actions = Vec::new();
    let mut commands = Vec::new();
    for item in items {
        match item {
            ResponseItem::Message { role, content, .. } => {
                let text = content
                    .iter()
                    .filter_map(|chunk| match chunk {
                        ContentItem::InputText { text }
                        | ContentItem::OutputText { text } => Some(text.trim()),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
                    .join(" ");
                if text.is_empty() {
                    continue;
                }
                actions.push(format!("{}: {}", role, text));
                if role == "assistant" {
                    if let Some(cmd) = text.lines().find(|line| line.trim_start().starts_with('$')) {
                        commands.push(cmd.trim().to_string());
                    }
                }
            }
            ResponseItem::FunctionCall { name, .. } => {
                actions.push(format!("Tool call: {name}"));
            }
            ResponseItem::FunctionCallOutput { output, .. } => {
                actions.push(format!("Tool output: {}", output.content));
            }
            _ => {}
        }
    }

    let mut lines = Vec::new();
    if let Some(prev) = prev_summary.filter(|text| !text.trim().is_empty()) {
        lines.push(format!("Building on previous checkpoint: {}", prev));
    }
    lines.push(format!(
        "Checkpoint covers {} exchanges and {} tool events.",
        actions.len(),
        items.iter().filter(|item| matches!(item, ResponseItem::FunctionCall { .. })).count()
    ));
    if !commands.is_empty() {
        let display = commands
            .into_iter()
            .take(MAX_COMMANDS_IN_SUMMARY)
            .collect::<Vec<_>>()
            .join(" | ");
        lines.push(format!("Key commands: {}", display));
    }
    if !actions.is_empty() {
        let display = actions
            .into_iter()
            .take(MAX_ACTION_LINES)
            .collect::<Vec<_>>()
            .join(" \n");
        lines.push(display);
    }
    lines.join("\n\n")
}

fn flatten_items(items: &[ResponseItem]) -> String {
    let mut buf = String::new();
    for item in items {
        match item {
            ResponseItem::Message { role, content, .. } => {
                let text = content
                    .iter()
                    .filter_map(|chunk| match chunk {
                        ContentItem::InputText { text }
                        | ContentItem::OutputText { text } => Some(text.as_str()),
                        ContentItem::InputImage { .. } => Some("<image>"),
                    })
                    .collect::<Vec<_>>()
                    .join(" ");
                if text.is_empty() {
                    continue;
                }
                buf.push_str(&format!("{role}: {text}\n"));
            }
            ResponseItem::FunctionCall { name, arguments, .. } => {
                buf.push_str(&format!("tool_call {name}: {arguments}\n"));
            }
            ResponseItem::FunctionCallOutput { output, .. } => {
                buf.push_str(&format!("tool_output: {}\n", output.content));
            }
            ResponseItem::CustomToolCall { name, input, .. } => {
                buf.push_str(&format!("custom_tool {name}: {input}\n"));
            }
            ResponseItem::CustomToolCallOutput { output, .. } => {
                buf.push_str(&format!("custom_tool_output: {}\n", output));
            }
            ResponseItem::Reasoning { summary, .. } => {
                for item in summary {
            match item {
                code_protocol::models::ReasoningItemReasoningSummary::SummaryText { text } => {
                    buf.push_str(&format!("reasoning: {text}\n"));
                }
            }
                }
            }
            _ => {}
        }
        if buf.len() >= MAX_TRANSCRIPT_BYTES {
            break;
        }
    }
    buf.truncate(min(buf.len(), MAX_TRANSCRIPT_BYTES));
    buf
}

fn advance_to_turn_boundary(items: &[ResponseItem], start_idx: usize) -> usize {
    let mut idx = start_idx;
    while idx < items.len() {
        if matches!(&items[idx], ResponseItem::Message { role, .. } if role == "user") {
            break;
        }
        idx += 1;
    }
    idx
}

pub(crate) fn estimate_item_tokens(item: &ResponseItem) -> usize {
    let byte_count = match item {
        ResponseItem::Message { content, .. } => content
            .iter()
            .map(|chunk| match chunk {
                ContentItem::InputText { text } | ContentItem::OutputText { text } => text.len(),
                ContentItem::InputImage { image_url } => image_url.len() / 10,
            })
            .sum(),
        ResponseItem::FunctionCall { name, arguments, .. } => name.len() + arguments.len(),
        ResponseItem::FunctionCallOutput { output, .. } => output.content.len(),
        ResponseItem::CustomToolCall { name, input, .. } => name.len() + input.len(),
        ResponseItem::CustomToolCallOutput { output, .. } => output.len(),
        ResponseItem::Reasoning { summary, content, .. } => {
            summary
                .iter()
                .map(|s| match s {
                    code_protocol::models::ReasoningItemReasoningSummary::SummaryText { text } => text.len(),
                })
                .sum::<usize>()
                + content
                    .as_ref()
                    .map(|segments| {
                        segments
                            .iter()
                            .map(|segment| match segment {
                                code_protocol::models::ReasoningItemContent::ReasoningText { text }
                                | code_protocol::models::ReasoningItemContent::Text { text } => text.len(),
                            })
                            .sum::<usize>()
                    })
                    .unwrap_or(0)
        }
        _ => 0,
    };
    byte_count.div_ceil(BYTES_PER_TOKEN)
}

fn make_checkpoint_message(text: String) -> ResponseItem {
    plain_message("user", format!("[CHECKPOINT SUMMARY]\n\n{}", text))
}

fn plain_message(role: &str, text: String) -> ResponseItem {
    ResponseItem::Message {
        id: None,
        role: role.to_string(),
        content: vec![ContentItem::InputText { text }],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn user_message(text: &str) -> ResponseItem {
        plain_message("user", text.to_string())
    }

    fn assistant_message(text: &str) -> ResponseItem {
        plain_message("assistant", text.to_string())
    }

    fn system_message(text: &str) -> ResponseItem {
        plain_message("system", text.to_string())
    }

    #[test]
    fn computes_slice_bounds_midpoint() {
        let conversation = vec![
            system_message("System"),
            user_message("Goal"),
            assistant_message("Step 1"),
            user_message("Step 2"),
            assistant_message("Step 2 done"),
            user_message("Step 3"),
        ];

        let (start, end) = compute_slice_bounds(&conversation).expect("bounds");
        assert_eq!(start, 2);
        assert_eq!(end, 5);
    }

    #[test]
    fn apply_compaction_preserves_goal() {
        let mut conversation = vec![
            system_message("System"),
            user_message("Goal"),
            assistant_message("Old content"),
            user_message("More content"),
            assistant_message("Final"),
        ];

        let summary = make_checkpoint_message("Summary".to_string());
        apply_compaction(&mut conversation, (2, 5), Some("Prev"), summary).expect("compaction");

        assert_eq!(conversation.len(), 4);
        assert!(matches!(&conversation[1], ResponseItem::Message { role, .. } if role == "user"));
        assert!(matches!(&conversation[2], ResponseItem::Message { .. }));
    }

    #[test]
    fn apply_compaction_inserts_prev_summary() {
        let mut conversation = vec![
            system_message("System"),
            user_message("Goal"),
            assistant_message("Old"),
            user_message("Tail"),
        ];

        let summary = make_checkpoint_message("New summary".to_string());
        apply_compaction(&mut conversation, (2, 4), Some("Prev summary"), summary).expect("compaction");

        assert_eq!(conversation.len(), 4);
        let prev = &conversation[2];
        if let ResponseItem::Message { content, .. } = prev {
            let joined = content
                .iter()
                .filter_map(|chunk| match chunk {
                    ContentItem::InputText { text } | ContentItem::OutputText { text } => {
                        Some(text.as_str())
                    }
                    _ => None,
                })
                .collect::<Vec<_>>()
                .join(" ");
            assert!(joined.contains("Prev summary"));
        } else {
            panic!("expected message");
        }
    }
}

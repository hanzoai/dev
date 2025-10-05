use std::collections::VecDeque;

use code_protocol::models::{ContentItem, ResponseItem};

/// Maintains the Auto Drive conversation transcript between coordinator turns.
///
/// `converted` mirrors what we previously derived from UI history and is used
/// when re-seeding the coordinator conversation. `raw` captures the exact
/// ResponseItems returned by the Auto Drive model so we can retain full
/// reasoning output without depending on UI rendering.
pub(crate) struct AutoDriveHistory {
    converted: Vec<ResponseItem>,
    raw: Vec<ResponseItem>,
    pending_duplicates: VecDeque<NormalizedMessage>,
}

impl AutoDriveHistory {
    pub(crate) fn new() -> Self {
        Self {
            converted: Vec::new(),
            raw: Vec::new(),
            pending_duplicates: VecDeque::new(),
        }
    }

    /// Replace the stored converted transcript. Returns any new tail items that
    /// were not present previously, preserving insertion order.
    pub(crate) fn replace_converted(&mut self, items: Vec<ResponseItem>) -> Vec<ResponseItem> {
        let prev_len = self.converted.len();
        self.converted = items;
        let tail: Vec<_> = if self.converted.len() <= prev_len {
            Vec::new()
        } else {
            self.converted
                .iter()
                .skip(prev_len)
                .cloned()
                .collect()
        };

        if tail.is_empty() {
            return tail;
        }

        if self.should_skip_entire_tail(&tail) {
            self.pending_duplicates.clear();
            return Vec::new();
        }

        if self.pending_duplicates.is_empty() {
            return tail;
        }

        let mut filtered = Vec::with_capacity(tail.len());
        let queue = &mut self.pending_duplicates;
        for item in tail.into_iter() {
            let matched = normalize_message(&item)
                .and_then(|message| queue.front().map(|expected| (message, expected)))
                .map(|(message, expected)| message == *expected)
                .unwrap_or(false);

            if matched {
                queue.pop_front();
                continue;
            }

            if queue.front().is_some() {
                queue.clear();
            }

            filtered.push(item);
        }

        filtered
    }

    fn should_skip_entire_tail(&self, tail: &[ResponseItem]) -> bool {
        if self.pending_duplicates.is_empty() {
            return false;
        }

        if tail.len() != self.pending_duplicates.len().saturating_add(1) {
            return false;
        }

        let first_is_user = matches!(tail.first(), Some(ResponseItem::Message { role, .. }) if role == "user");
        if !first_is_user {
            return false;
        }

        tail.iter()
            .skip(1)
            .zip(self.pending_duplicates.iter())
            .all(|(item, expected)| {
                let Some(message) = normalize_message(item) else {
                    return false;
                };
                if message.role != expected.role {
                    return false;
                }

                let item_segments: Vec<&str> = message
                    .content
                    .iter()
                    .filter_map(content_text)
                    .collect();
                let expected_segments: Vec<&str> = expected
                    .content
                    .iter()
                    .filter_map(content_text)
                    .collect();

                item_segments == expected_segments
            })
    }

    pub(crate) fn append_raw(&mut self, items: &[ResponseItem]) {
        if items.is_empty() {
            return;
        }
        self.raw.extend(items.iter().cloned());
        for item in items.iter() {
            if let Some(message) = normalize_message(item) {
                self.pending_duplicates.push_back(message);
            }
        }
    }

    pub(crate) fn append_converted_tail(&mut self, items: &[ResponseItem]) {
        if items.is_empty() {
            return;
        }
        self.raw.extend(items.iter().cloned());
    }

    pub(crate) fn raw_snapshot(&self) -> Vec<ResponseItem> {
        self.raw.clone()
    }

    pub(crate) fn clear(&mut self) {
        self.converted.clear();
        self.raw.clear();
        self.pending_duplicates.clear();
    }

    pub(crate) fn converted_is_empty(&self) -> bool {
        self.converted.is_empty()
    }

}


#[derive(Clone, Debug, PartialEq, Eq)]
struct NormalizedMessage {
    role: String,
    content: Vec<NormalizedContent>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum NormalizedContent {
    InputText(String),
    OutputText(String),
    InputImage(String),
}

fn normalize_message(item: &ResponseItem) -> Option<NormalizedMessage> {
    if let ResponseItem::Message { role, content, .. } = item {
        let normalized = content
            .iter()
            .map(|chunk| match chunk {
                ContentItem::InputText { text } => NormalizedContent::InputText(text.clone()),
                ContentItem::OutputText { text } => NormalizedContent::OutputText(text.clone()),
                ContentItem::InputImage { image_url } => {
                    NormalizedContent::InputImage(image_url.clone())
                }
            })
            .collect();
        Some(NormalizedMessage {
            role: role.clone(),
            content: normalized,
        })
    } else {
        None
    }
}

fn content_text(content: &NormalizedContent) -> Option<&str> {
    match content {
        NormalizedContent::InputText(text)
        | NormalizedContent::OutputText(text)
        | NormalizedContent::InputImage(text) => Some(text.as_str()),
    }
}

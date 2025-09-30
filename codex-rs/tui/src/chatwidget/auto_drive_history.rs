use codex_protocol::models::ResponseItem;

/// Maintains the Auto Drive conversation transcript between coordinator turns.
///
/// `converted` mirrors what we previously derived from UI history and is used
/// when re-seeding the coordinator conversation. `raw` captures the exact
/// ResponseItems returned by the Auto Drive model so we can retain full
/// reasoning output without depending on UI rendering.
pub(crate) struct AutoDriveHistory {
    converted: Vec<ResponseItem>,
    raw: Vec<ResponseItem>,
    pending_skip: usize,
}

impl AutoDriveHistory {
    pub(crate) fn new() -> Self {
        Self {
            converted: Vec::new(),
            raw: Vec::new(),
            pending_skip: 0,
        }
    }

    /// Replace the stored converted transcript. Returns any new tail items that
    /// were not present previously, preserving insertion order.
    pub(crate) fn replace_converted(&mut self, items: Vec<ResponseItem>) -> Vec<ResponseItem> {
        let prev_len = self.converted.len();
        self.converted = items;
        let mut tail: Vec<_> = if self.converted.len() <= prev_len {
            Vec::new()
        } else {
            self.converted
                .iter()
                .skip(prev_len)
                .cloned()
                .collect()
        };

        if !tail.is_empty() && self.pending_skip > 0 {
            if self.pending_skip >= tail.len() {
                self.pending_skip -= tail.len();
                tail.clear();
            } else {
                tail.drain(0..self.pending_skip);
                self.pending_skip = 0;
            }
        }

        tail
    }

    pub(crate) fn append_raw(&mut self, items: &[ResponseItem]) {
        if items.is_empty() {
            return;
        }
        self.raw.extend(items.iter().cloned());
        self.pending_skip = self.pending_skip.saturating_add(items.len());
    }

    pub(crate) fn raw_snapshot(&self) -> Vec<ResponseItem> {
        self.raw.clone()
    }

    pub(crate) fn clear(&mut self) {
        self.converted.clear();
        self.raw.clear();
        self.pending_skip = 0;
    }

    pub(crate) fn converted_is_empty(&self) -> bool {
        self.converted.is_empty()
    }

    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn raw_items(&self) -> &[ResponseItem] {
        &self.raw
    }
}

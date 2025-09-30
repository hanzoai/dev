use codex_protocol::models::ResponseItem;

/// Maintains the Auto Drive conversation transcript between coordinator turns.
pub(crate) struct AutoDriveHistory {
    items: Vec<ResponseItem>,
}

impl AutoDriveHistory {
    pub(crate) fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Replace the stored transcript with the provided items.
    pub(crate) fn replace(&mut self, items: Vec<ResponseItem>) {
        self.items = items;
    }

    pub(crate) fn snapshot(&self) -> Vec<ResponseItem> {
        self.items.clone()
    }

    pub(crate) fn clear(&mut self) {
        self.items.clear();
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

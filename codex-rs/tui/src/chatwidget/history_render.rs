use std::cell::{Cell, RefCell};
use std::collections::HashMap;

/// Memoized layout data for history rendering.
pub(crate) struct HistoryRenderState {
    pub(crate) height_cache: RefCell<HashMap<(usize, u16), u16>>,
    pub(crate) height_cache_last_width: Cell<u16>,
    pub(crate) prefix_sums: RefCell<Vec<u16>>,
    pub(crate) last_prefix_width: Cell<u16>,
    pub(crate) last_prefix_count: Cell<usize>,
    pub(crate) prefix_valid: Cell<bool>,
}

impl HistoryRenderState {
    pub(crate) fn new() -> Self {
        Self {
            height_cache: RefCell::new(HashMap::new()),
            height_cache_last_width: Cell::new(0),
            prefix_sums: RefCell::new(Vec::new()),
            last_prefix_width: Cell::new(0),
            last_prefix_count: Cell::new(0),
            prefix_valid: Cell::new(false),
        }
    }

    pub(crate) fn invalidate_height_cache(&self) {
        self.height_cache.borrow_mut().clear();
        self.prefix_sums.borrow_mut().clear();
        self.prefix_valid.set(false);
    }

    pub(crate) fn handle_width_change(&self, width: u16) {
        if self.height_cache_last_width.get() != width {
            self.height_cache.borrow_mut().clear();
            self.prefix_sums.borrow_mut().clear();
            self.prefix_valid.set(false);
            self.height_cache_last_width.set(width);
        }
    }
}

impl Default for HistoryRenderState {
    fn default() -> Self {
        Self::new()
    }
}

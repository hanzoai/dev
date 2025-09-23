use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use ratatui::text::Line;

use crate::insert_history::word_wrap_lines;

/// Memoized layout data for history rendering.
pub(crate) struct HistoryRenderState {
    pub(crate) layout_cache: RefCell<HashMap<(usize, u16), Rc<Vec<Line<'static>>>>>,
    pub(crate) height_cache_last_width: Cell<u16>,
    pub(crate) prefix_sums: RefCell<Vec<u16>>,
    pub(crate) last_prefix_width: Cell<u16>,
    pub(crate) last_prefix_count: Cell<usize>,
    pub(crate) prefix_valid: Cell<bool>,
}

impl HistoryRenderState {
    pub(crate) fn new() -> Self {
        Self {
            layout_cache: RefCell::new(HashMap::new()),
            height_cache_last_width: Cell::new(0),
            prefix_sums: RefCell::new(Vec::new()),
            last_prefix_width: Cell::new(0),
            last_prefix_count: Cell::new(0),
            prefix_valid: Cell::new(false),
        }
    }

    pub(crate) fn invalidate_height_cache(&self) {
        self.layout_cache.borrow_mut().clear();
        self.prefix_sums.borrow_mut().clear();
        self.prefix_valid.set(false);
    }

    pub(crate) fn handle_width_change(&self, width: u16) {
        if self.height_cache_last_width.get() != width {
            self.layout_cache.borrow_mut().clear();
            self.prefix_sums.borrow_mut().clear();
            self.prefix_valid.set(false);
            self.height_cache_last_width.set(width);
        }
    }

    pub(crate) fn ensure_layout<F>(
        &self,
        idx: usize,
        width: u16,
        build_lines: F,
    ) -> LayoutRef
    where
        F: FnOnce() -> Vec<Line<'static>>,
    {
        if width == 0 {
            return LayoutRef {
                lines: Rc::new(Vec::new()),
                freshly_computed: false,
            };
        }

        let key = (idx, width);
        if let Some(layout) = self.layout_cache.borrow().get(&key).cloned() {
            return LayoutRef {
                lines: layout,
                freshly_computed: false,
            };
        }

        let lines = build_lines();
        let wrapped = if lines.is_empty() {
            Vec::new()
        } else {
            word_wrap_lines(&lines, width)
        };
        let layout = Rc::new(wrapped);
        self.layout_cache
            .borrow_mut()
            .insert(key, Rc::clone(&layout));
        LayoutRef {
            lines: layout,
            freshly_computed: true,
        }
    }
}

#[derive(Clone)]
pub(crate) struct LayoutRef {
    pub(crate) lines: Rc<Vec<Line<'static>>>,
    pub(crate) freshly_computed: bool,
}

impl Default for HistoryRenderState {
    fn default() -> Self {
        Self::new()
    }
}

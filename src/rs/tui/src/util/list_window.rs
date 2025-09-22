//! List window utility for TUI

use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct ListWindow<T> {
    items: VecDeque<T>,
    window_size: usize,
    offset: usize,
}

impl<T> ListWindow<T> {
    pub fn new(window_size: usize) -> Self {
        Self {
            items: VecDeque::new(),
            window_size,
            offset: 0,
        }
    }

    pub fn push(&mut self, item: T) {
        self.items.push_back(item);
        if self.items.len() > self.window_size * 10 {
            self.items.pop_front();
        }
    }

    pub fn visible_items(&self) -> impl Iterator<Item = &T> {
        self.items
            .iter()
            .skip(self.offset)
            .take(self.window_size)
    }

    pub fn scroll_up(&mut self) {
        self.offset = self.offset.saturating_sub(1);
    }

    pub fn scroll_down(&mut self) {
        let max_offset = self.items.len().saturating_sub(self.window_size);
        if self.offset < max_offset {
            self.offset += 1;
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

/// Create an anchored window for list display
pub fn anchored_window(
    total_items: usize,
    window_size: usize,
    selected_index: usize,
    anchor: Option<usize>,
) -> (usize, usize) {
    use std::cmp;

    if total_items == 0 {
        return (0, 0);
    }

    let window_size = cmp::min(window_size, total_items);
    let anchor = anchor.unwrap_or(0);

    // Calculate the window start based on selected index
    let start = if selected_index < anchor {
        selected_index
    } else if selected_index >= anchor + window_size {
        selected_index - window_size + 1
    } else {
        anchor
    };

    let start = cmp::min(start, total_items.saturating_sub(window_size));
    let end = cmp::min(start + window_size, total_items);

    (start, end)
}
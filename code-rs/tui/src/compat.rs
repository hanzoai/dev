//! Compatibility layer for ratatui
//!
//! This module re-exports all ratatui types used throughout the TUI codebase.
//! By importing from this module instead of ratatui directly, we can more
//! easily migrate to a different TUI library in the future if needed.

// Buffer types
pub use ratatui::buffer::Buffer;

// Layout types
pub use ratatui::layout::{Alignment, Constraint, Layout, Margin, Rect};

// Style types
pub use ratatui::style::{Color, Modifier, Style, Styled, Stylize};

// Text types
pub use ratatui::text::{Line, Span, Text};

// Widget types
pub use ratatui::widgets::{
    Block, Borders, Cell, Clear, Padding, Paragraph, Row, StatefulWidgetRef, Table, Tabs, Widget,
    WidgetRef, Wrap,
};

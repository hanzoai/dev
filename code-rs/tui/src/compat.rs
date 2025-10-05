//! Compatibility layer for ratatui types.
//!
//! Re-export commonly used ratatui types so the rest of the crate can import
//! from `crate::compat::*`. This keeps our call sites insulated from upstream
//! renames or module moves.

// Core rendering primitives
pub use ratatui::buffer::Buffer;
pub use ratatui::layout::{Alignment, Constraint, Layout, Margin, Rect};
pub use ratatui::style::{Color, Modifier, Style, Stylize};
pub use ratatui::text::{Line, Span};
#[cfg(test)]
pub use ratatui::text::Text;

// Widget traits and common widgets
pub use ratatui::widgets::{
    Block, Borders, Cell, Clear, Paragraph, Row, StatefulWidgetRef, Table, Widget, WidgetRef,
    Wrap,
};

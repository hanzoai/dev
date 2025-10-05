# TUI Adapter Status (Archived)

We briefly introduced `code-rs/tui/src/compat.rs` as a thin layer that
re-exported ratatui types (
`Buffer`, `Line`, `Rect`, `Style`, `Widget`, …) to ease mass refactors. The
experiment proved unnecessary once the module system stabilized, so the adapter
was removed on **2025-10-06**.

All call sites now import directly from ratatui, e.g.:

```rust
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::text::Line;
use ratatui::widgets::Widget;
```

This document is kept only as historical context; no further action is required.

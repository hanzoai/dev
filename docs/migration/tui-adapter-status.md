# TUI Adapter Migration Status

## Overview

This document tracks the migration of `code-rs/tui` modules from direct `ratatui` imports to the internal `compat` adapter module. The adapter provides a stable internal API that insulates the codebase from upstream breaking changes.

## Adapter Module

**Location:** `code-rs/tui/src/compat.rs`

The compat module re-exports commonly-used ratatui types:

- **Core rendering**: `Buffer`, `Rect`, `Layout`, `Constraint`, `Alignment`, `Margin`
- **Styling**: `Color`, `Style`, `Modifier`, `Stylize`
- **Text**: `Line`, `Span`, `Text` (Text only available under `#[cfg(test)]`)
- **Widgets**: `Widget`, `WidgetRef`, `StatefulWidgetRef`, `Paragraph`, `Block`, `Borders`, `Clear`, `Table`, `Row`, `Cell`, `Wrap`

**Note:** Additional types will be added to the compat module as migration progresses. Unused re-exports are avoided to maintain zero-warning builds.

## Migration Pattern

### Before
```rust
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::text::Line;
use ratatui::widgets::Widget;
```

### After
```rust
use crate::compat::{Buffer, Line, Rect, Style, Widget};
```

## Migrated Modules (Representative Sample)

The following modules have been updated to use the compat adapter:

1. `src/compat.rs` - **New adapter module**
2. `src/lib.rs` - Added `mod compat;` declaration
3. `src/colors.rs` - Color utilities
4. `src/height_manager.rs` - Layout management
5. `src/render/line_utils.rs` - Line manipulation helpers
6. `src/markdown_render_tests.rs` - Test module
7. `src/onboarding/onboarding_screen.rs` - Onboarding coordinator
8. `src/onboarding/trust_directory.rs` - Trust directory widget
9. `src/bottom_pane/selection_popup_common.rs` - Selection UI helpers
10. `src/bottom_pane/chat_composer.rs` - Chat input composer
11. `src/bottom_pane/mcp_settings_view.rs` - MCP settings UI

These represent approximately **13%** of the 84 files with direct ratatui imports, covering:
- Core utilities (colors, height_manager, render/line_utils)
- Onboarding flow widgets
- Bottom pane UI components
- Test modules

## Remaining Work

### Coverage Analysis

**Total files with ratatui imports:** 84
**Files migrated:** 11 (including compat.rs)
**Files remaining:** 73

### Most Impactful Remaining Modules

Based on import frequency, the following modules should be prioritized:

1. `src/chatwidget.rs` (30+ imports) - Main chat widget
2. `src/chatwidget/*.rs` submodules - Chat widget components
3. `src/history_cell/*.rs` modules - History cell renderers
4. `src/bottom_pane/*.rs` (remaining ~15 files) - UI panels
5. `src/markdown_render.rs` - Markdown rendering
6. `src/diff_render.rs` - Diff visualization
7. `src/app.rs` - Main app coordinator

### Special Cases

The following patterns require careful handling:

**Renamed imports (keep as-is for now):**
```rust
use ratatui::text::Line as RtLine;
use ratatui::text::Span as RtSpan;
use ratatui::text::Text as RtText;
```

**Prelude wildcards (migrate case-by-case):**
```rust
use ratatui::prelude::*;  // Evaluate if compat::prelude or specific imports needed
```

**Scrollbar symbols:**
```rust
use ratatui::symbols::scrollbar as scrollbar_symbols;  // Already in compat
```

## Migration Strategy

### Phase 1: Foundation (✅ Complete)
- Create `compat.rs` adapter module
- Update representative sample across module categories
- Validate pattern with surgical refactor

### Phase 2: Systematic Migration (Recommended Next Steps)
1. Update all `bottom_pane/*.rs` modules (consistency in UI layer)
2. Update all `chatwidget/*.rs` modules (largest subsystem)
3. Update all `history_cell/*.rs` modules (rendering pipeline)
4. Update `markdown_render.rs` and related markdown modules
5. Update `app.rs` and top-level coordination modules

### Phase 3: Cleanup
- Search for any remaining `use ratatui::` imports
- Update renamed imports (RtLine, RtSpan, RtText) if beneficial
- Consider prelude migration strategy

## Build Validation

After each batch of updates, run:

```bash
cargo check --lib              # Fast check for library code only
./build-fast.sh --workspace code  # Full build including binaries
```

Expect: **Zero warnings**, clean build.

### Phase 1 Validation Results

```bash
$ cargo check --lib
   Finished `dev` profile [unoptimized + debuginfo] target(s) in 3.13s
```

✅ **Zero warnings**, clean compilation confirmed for library code.

## Notes

- Keep changes surgical: modify only import statements, no logic changes
- Maintain alphabetical ordering in multi-line import blocks
- Prefer explicit imports over wildcards (except where prelude is clearly needed)
- The adapter pattern allows future migration to a different TUI library with minimal code churn

## Last Updated

**Date:** 2025-10-05
**Branch:** code-claude-audit-code-rs-tui-modules
**Status:** Phase 1 complete, Phase 2 ready to begin

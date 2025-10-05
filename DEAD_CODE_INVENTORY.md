# Dead Code Inventory and Deletion Plan for code-rs

**Generated:** 2025-10-05
**Scope:** code-rs/tui, code-rs/core, and related crates
**Purpose:** Identify and plan removal of dead compatibility scaffolding, orphaned modules, and unused code

---

## Executive Summary

This document provides a comprehensive inventory of dead or unused code in the code-rs codebase, organized by category with specific deletion plans and verification steps.

### Key Findings

- **~11,200 lines** of dead test code behind `legacy_tests` feature flag
- **6 orphaned modules** totaling ~72,700 bytes
- **10+ entire modules** marked with `#![allow(dead_code)]`
- **1 unused feature flag** (`vt100-tests` in code-rs/tui)
- **0 orphaned prompt files** (all are actively used)
- **100+ stale comments** about removed code

---

## Category 1: Feature Flags

### 1.1 legacy_tests Feature Flag (HIGH PRIORITY)

**Location:** `code-rs/tui/Cargo.toml:20`

**Status:** BROKEN - Does not compile (106+ errors)

**Scope:**
- 2 dedicated test files: 4,985 lines
- 23 modules with gated test code: ~6,247 lines
- Total: **~11,232 lines of dead code**

**Files Affected:**
```
code-rs/tui/src/chatwidget/tests.rs (4,634 lines)
code-rs/tui/src/chatwidget/tests_retry.rs (351 lines)
code-rs/tui/src/text_formatting.rs (~164 lines of tests)
code-rs/tui/src/text_processing.rs (~24 lines of tests)
code-rs/tui/src/updates.rs (~23 lines of tests)
code-rs/tui/src/status_indicator_widget.rs (~53 lines of tests)
code-rs/tui/src/pager_overlay.rs (~209 lines of tests)
code-rs/tui/src/markdown_renderer.rs (~75 lines of tests)
code-rs/tui/src/markdown_stream.rs (~499 lines of tests)
code-rs/tui/src/markdown.rs (~251 lines of tests)
code-rs/tui/src/live_wrap.rs (~81 lines of tests)
code-rs/tui/src/insert_history.rs (~80 lines of tests)
code-rs/tui/src/exec_command.rs (~17 lines of tests)
code-rs/tui/src/diff_render.rs (~155 lines of tests)
code-rs/tui/src/clipboard_paste.rs (~115 lines of tests)
code-rs/tui/src/user_approval_widget.rs (~138 lines of tests)
code-rs/tui/src/backtrack_helpers.rs (~41 lines of tests)
code-rs/tui/src/streaming/controller.rs (~180 lines of tests)
code-rs/tui/src/bottom_pane/textarea.rs (~914 lines of tests)
code-rs/tui/src/bottom_pane/mod.rs (~256 lines of tests)
code-rs/tui/src/bottom_pane/chat_composer.rs (~751 lines of tests)
code-rs/tui/src/bottom_pane/chat_composer_history.rs (~61 lines of tests)
code-rs/tui/src/bottom_pane/command_popup.rs (~319 lines of tests)
code-rs/tui/src/bottom_pane/scroll_state.rs (~28 lines of tests)
code-rs/tui/src/bottom_pane/approval_modal_view.rs (~37 lines of tests)
```

**Deletion Plan:**

1. **Delete entire test files:**
   ```bash
   rm code-rs/tui/src/chatwidget/tests.rs
   rm code-rs/tui/src/chatwidget/tests_retry.rs
   ```

2. **Remove gated test modules from each file:**
   Search for `#[cfg(all(test, feature = "legacy_tests"))]` and delete the entire module block in each of the 23 files listed above.

3. **Remove feature flag from Cargo.toml:**
   ```bash
   # Edit code-rs/tui/Cargo.toml, delete lines 19-20:
   # # Disable legacy tests by default; enable with `--features legacy_tests` if needed.
   # legacy_tests = []
   ```

4. **Remove test-only helper functions:**
   - `simulate_stream_markdown_for_tests()` in `markdown_stream.rs`
   - `CommandPopup::new()` test constructor
   - `ChatWidget::test_for_request()` constructor

**Verification Steps:**

```bash
# 1. Verify feature is not in CI/CD
grep -r "legacy_tests" .github/ scripts/ justfile

# 2. Attempt build before deletion (should succeed)
cd code-rs/tui && cargo build

# 3. Perform deletion

# 4. Verify build still succeeds
cargo build

# 5. Verify tests run (non-legacy tests)
cargo test

# 6. Check for any remaining references
rg "legacy_tests" code-rs/
```

**Risk:** LOW - Code is already broken and unused

---

### 1.2 vt100-tests Feature Flag (MEDIUM PRIORITY)

**Location:** `code-rs/tui/Cargo.toml:16`

**Status:** UNUSED - No tests exist in code-rs/tui (tests exist in codex-rs/tui)

**Deletion Plan:**

```bash
# Remove feature flag definition
# Edit code-rs/tui/Cargo.toml, delete line 16:
# vt100-tests = []
```

**Note:** Keep `vt100` dependency in main dependencies (line 104) as it's used in production code (`chatwidget/terminal.rs`).

**Verification Steps:**

```bash
# 1. Verify no tests use this feature
find code-rs/tui/tests -name "*.rs" 2>/dev/null

# 2. Check if vt100 is used in production
rg "use vt100" code-rs/tui/src/

# 3. Remove feature flag

# 4. Verify build
cargo build -p code-tui
```

**Risk:** LOW - Feature has no tests

---

## Category 2: Orphaned Modules and Files

### 2.1 Confirmed Orphaned Files (code-rs/tui)

#### 2.1.1 text_block.rs

**Location:** `code-rs/tui/src/text_block.rs`
**Size:** 278 bytes
**Status:** Commented out in lib.rs:73 with note "Orphaned after trait-based HistoryCell migration"

**Deletion Plan:**
```bash
rm code-rs/tui/src/text_block.rs
# Also remove commented line from code-rs/tui/src/lib.rs:73
```

**Verification:**
```bash
rg "text_block" code-rs/tui/
cargo build -p code-tui
```

**Risk:** NONE - Already acknowledged as orphaned

---

#### 2.1.2 scroll_view.rs

**Location:** `code-rs/tui/src/scroll_view.rs`
**Size:** 4,529 bytes
**Status:** Commented out in lib.rs:63 with note "Orphaned after trait-based HistoryCell migration"

**Deletion Plan:**
```bash
rm code-rs/tui/src/scroll_view.rs
# Also remove commented line from code-rs/tui/src/lib.rs:63
```

**Verification:**
```bash
rg "scroll_view" code-rs/tui/
cargo build -p code-tui
```

**Risk:** NONE - Already acknowledged as orphaned

---

#### 2.1.3 app_backtrack.rs

**Location:** `code-rs/tui/src/app_backtrack.rs`
**Size:** 14,257 bytes
**Status:** Not declared in any mod.rs/lib.rs

**Deletion Plan:**
```bash
rm code-rs/tui/src/app_backtrack.rs
```

**Verification:**
```bash
# Check for any references to BacktrackState
rg "BacktrackState" code-rs/tui/
rg "app_backtrack" code-rs/tui/
cargo build -p code-tui
```

**Risk:** LOW - No module declaration found

---

#### 2.1.4 resume_picker.rs

**Location:** `code-rs/tui/src/resume_picker.rs`
**Size:** 37,218 bytes
**Status:** Not declared in module tree (only `mod resume` exists)

**Note:** Depends on `custom_terminal.rs` which is also orphaned

**Deletion Plan:**
```bash
rm code-rs/tui/src/resume_picker.rs
```

**Verification:**
```bash
rg "resume_picker" code-rs/tui/
rg "PickerState" code-rs/tui/
cargo build -p code-tui
```

**Risk:** LOW - Not integrated into module tree

---

#### 2.1.5 custom_terminal.rs

**Location:** `code-rs/tui/src/custom_terminal.rs`
**Size:** 22,672 bytes
**Status:** USED by tests but not declared as module

**Note:** This is referenced by `resume_picker.rs` and `chatwidget/tests.rs` (which is behind legacy_tests)

**Deletion Plan:**

**Option A:** Delete if legacy_tests are removed:
```bash
rm code-rs/tui/src/custom_terminal.rs
```

**Option B:** Add module declaration if needed:
```rust
// In code-rs/tui/src/lib.rs, add:
#[cfg(test)]
mod custom_terminal;
```

**Verification:**
```bash
rg "custom_terminal" code-rs/tui/
cargo build -p code-tui
cargo test -p code-tui
```

**Risk:** MEDIUM - Used by test code, but those tests are broken

**Recommendation:** Delete along with legacy_tests removal

---

#### 2.1.6 exec_cell/ directory

**Location:** `code-rs/tui/src/exec_cell/`
**Contents:** `render.rs` (16,444 bytes)
**Status:** Orphaned - references non-existent `super::model::ExecCell`

**Note:** The actual ExecCell is in `history_cell/exec.rs`, this appears to be old/duplicate code

**Deletion Plan:**
```bash
rm -rf code-rs/tui/src/exec_cell/
```

**Verification:**
```bash
rg "exec_cell" code-rs/tui/
rg "crate::exec_cell" code-rs/tui/
cargo build -p code-tui
```

**Risk:** LOW - No references found

---

### 2.2 Module Declaration Issues (code-rs/core)

#### 2.2.1 tasks/ directory

**Location:** `code-rs/core/src/tasks/`
**Contents:** `mod.rs`, `compact.rs`, `regular.rs`, `review.rs`
**Status:** USED but not declared as module

**Issue:** Code in `state/turn.rs` references `use crate::tasks::SessionTask;` but `mod tasks;` is missing from lib.rs

**Action Required:** ADD MODULE DECLARATION (not removal)

```rust
// In code-rs/core/src/lib.rs, add:
pub mod tasks;
```

**Verification:**
```bash
cargo build -p code-core
cargo test -p code-core
```

**Risk:** NONE - This fixes a bug

---

#### 2.2.2 tests/suite/otel.rs

**Location:** `code-rs/core/tests/suite/otel.rs`
**Status:** Test file not declared in `tests/suite/mod.rs`

**Action Required:** Either add to mod.rs or delete

**Option A:** Add to suite:
```rust
// In code-rs/core/tests/suite/mod.rs, add:
mod otel;
```

**Option B:** Delete if unused:
```bash
rm code-rs/core/tests/suite/otel.rs
```

**Verification:**
```bash
cargo test -p code-core --test suite
```

---

## Category 3: Modules Marked #[allow(dead_code)]

These entire modules are marked with `#![allow(dead_code)]` at the file level, indicating they may be completely unused:

### 3.1 High Priority Candidates (code-rs/tui)

| File | Size | Status |
|------|------|--------|
| `src/pager_overlay.rs` | 25,058 B | `#![allow(dead_code, unused_imports, unused_variables)]` |
| `src/streaming/controller.rs` | 18,450 B | `#![allow(dead_code)]` |
| `src/streaming/mod.rs` | 3,952 B | `#![allow(dead_code)]` |
| `src/markdown_stream.rs` | 41,063 B | `#![allow(dead_code)]` |
| `src/markdown.rs` | 28,666 B | `#![allow(dead_code)]` |
| `src/transcript_app.rs` | 9,904 B | `#![allow(dead_code)]` |
| `src/backtrack_helpers.rs` | 4,919 B | `#![allow(dead_code)]` |
| `src/bottom_pane/list_selection_view.rs` | ? | `#![allow(dead_code)]` |
| `src/bottom_pane/paste_burst.rs` | ? | `#![allow(dead_code, unused_imports, unused_variables)]` |

**Total:** ~131,000+ bytes

### 3.2 Core Modules (code-rs/core)

| File | Status |
|------|--------|
| `src/acp.rs` | `#![allow(dead_code)]` |
| `src/function_tool.rs` | `#[allow(dead_code)]` on module |

**Investigation Plan:**

For each file:

1. **Check for external references:**
   ```bash
   # Example for pager_overlay.rs
   rg "pager_overlay" code-rs/ --type rust | grep -v "src/pager_overlay.rs"
   rg "PagerOverlay" code-rs/ --type rust | grep -v "src/pager_overlay.rs"
   ```

2. **Try compilation without file:**
   ```bash
   # Temporarily rename file
   mv src/pager_overlay.rs src/pager_overlay.rs.bak
   cargo build -p code-tui
   # If build succeeds, the file is safe to delete
   ```

3. **Check git history:**
   ```bash
   git log --oneline --follow src/pager_overlay.rs | head -20
   ```

**Verification Process:**

```bash
# For each potentially dead module:

# 1. Search for usage
rg "ModuleName" code-rs/

# 2. Comment out module declaration in lib.rs
# (or rename file to .bak)

# 3. Build
cargo build

# 4. Test
cargo test

# 5. If successful, proceed with deletion
```

**Risk:** MEDIUM - Requires individual verification of each module

---

## Category 4: Stale Comments

### 4.1 Comments About Removed Code (Safe to Delete)

These are comments documenting code that has already been removed:

**code-rs/core/src/codex.rs:**
- Line 9010: `// removed upstream exit_review_mode helper: not used in fork`
- Lines 2539, 2542, 2554, 2556, 2564: `// (debug removed)`
- Line 2884: `// (submission diagnostics removed)`

**code-rs/core/src/client.rs:**
- Line 645: `// duplicate of earlier helpers removed during merge cleanup`

**code-rs/tui/src/chatwidget.rs:**
- Line 1235: `// Removed legacy turn-window logic; ordering is strictly global.`
- Line 2674: `// Legacy helper removed: streaming now requires explicit sequence numbers.`
- Line 10905: `// Legacy show_agents_settings_ui removed — overview/Direct editors replace it`
- Line 15695: `// removed legacy ensure_stream_order_key; strict variant is used instead`

**code-rs/tui/src/chatwidget/exec_tools.rs:**
- Line 1023: `// Stable ordering now inserts at the correct position; these helpers are removed.`

**Deletion Plan:**

```bash
# Manually remove these comments from the files listed above
# This is a code cleanup task with no functional impact
```

**Verification:** Visual inspection + `cargo build`

**Risk:** NONE

---

### 4.2 Comments to Keep (Important Documentation)

These comments document architectural decisions and should be retained:

- `code-rs/core/src/codex.rs:3843` - Documents intentional review flow divergence
- `code-rs/core/src/codex.rs:3545` - Documents upstream protocol changes
- `code-rs/core/src/client.rs:1212-1214` - Documents where to find removed helpers if needed
- `code-rs/core/src/exec.rs:35` - Documents architectural change
- All "legacy" config compatibility comments in `config_types.rs`

---

## Category 5: Documentation Files

### 5.1 Planning/Design Documents (Keep or Archive)

**Location:** `code-rs/tui/agent_tasks/`

**Files:**
- `plain_loading_wait_migration.md`
- `renderer_cache.md`
- `stream_exec_assistant_migration.md`

**Status:** Not referenced in code, appear to be design documents

**Recommendation:**

**Option A:** Archive to docs/archive/ directory:
```bash
mkdir -p docs/archive/tui-migrations
mv code-rs/tui/agent_tasks/*.md docs/archive/tui-migrations/
rmdir code-rs/tui/agent_tasks
```

**Option B:** Keep as-is if they contain useful migration context

**Option C:** Delete if outdated:
```bash
rm -rf code-rs/tui/agent_tasks/
```

**Verification:** Review content to determine historical value

**Risk:** LOW - Documentation only

---

### 5.2 Active Documentation (Keep)

**code-rs/tui/HISTORY_CELLS_PLAN.md** - Active planning doc, keep

**code-rs/tui/styles.md** - Developer reference, keep (or move to docs/)

---

## Category 6: Prompt Files

### Status: ALL ACTIVE

No orphaned prompt files found. All prompt files have confirmed `include_str!()` usage:

- ✅ `code-rs/core/gpt_5_code_prompt.md` - Used by gpt-5-codex models
- ✅ `code-rs/core/prompt.md` - Base instructions for all models
- ✅ `code-rs/core/prompt_coder.md` - Additional developer instructions
- ✅ `code-rs/core/review_prompt.md` - Review mode functionality
- ✅ `code-rs/tui/prompt_for_init_command.md` - /init slash command
- ✅ `code-rs/core/templates/compact/prompt.md` - Context compaction
- ✅ `code-rs/core/templates/compact/history_bridge.md` - Context compaction

**Action:** None required

---

## Deletion Priority Matrix

### P0 - Immediate (Zero Risk)

1. Delete `text_block.rs` + comment in lib.rs
2. Delete `scroll_view.rs` + comment in lib.rs
3. Delete stale comments in codex.rs, client.rs, chatwidget.rs
4. Fix module declaration for `tasks/` directory

**Estimated cleanup:** ~5,000 bytes + improved code clarity

---

### P1 - High Priority (Low Risk)

1. Remove `legacy_tests` feature flag and all gated code (~11,232 lines)
2. Delete `app_backtrack.rs`
3. Delete `resume_picker.rs`
4. Delete `custom_terminal.rs`
5. Delete `exec_cell/` directory
6. Remove `vt100-tests` feature flag from code-rs/tui

**Estimated cleanup:** ~11,300 lines + ~90KB

---

### P2 - Medium Priority (Requires Investigation)

1. Investigate and remove modules marked `#![allow(dead_code)]`
   - Start with `pager_overlay.rs`, `transcript_app.rs`, `backtrack_helpers.rs`
   - Then `streaming/` directory modules
   - Then `markdown.rs` and `markdown_stream.rs`
2. Fix or delete `tests/suite/otel.rs`
3. Archive or delete `agent_tasks/` directory

**Estimated cleanup:** ~131KB

---

### P3 - Low Priority (Documentation)

1. Review and archive old planning docs in `agent_tasks/`
2. Consider moving `styles.md` to docs/ directory

**Estimated cleanup:** Organizational only

---

## Verification Checklist

After each deletion phase:

```bash
# 1. Build succeeds
cargo build --workspace

# 2. Tests pass (non-legacy)
cargo test --workspace

# 3. Clippy is happy
cargo clippy --workspace -- -D warnings

# 4. Format is correct
cargo fmt --check

# 5. No new warnings
cargo build --workspace 2>&1 | grep -i warning

# 6. Git status is clean (except intended changes)
git status
```

---

## Rollback Plan

Before any deletions:

```bash
# Create a backup branch
git checkout -b backup-before-dead-code-cleanup
git push origin backup-before-dead-code-cleanup

# Create working branch
git checkout -b cleanup-dead-code-phase-1

# Perform deletions with individual commits
git commit -m "Remove legacy_tests feature and gated code"
git commit -m "Remove orphaned modules: text_block, scroll_view"
# etc.

# If issues arise, revert specific commits or reset to backup branch
```

---

## Success Metrics

- **Lines of code removed:** Target ~11,500+ lines
- **Files removed:** Target ~8-10 files
- **Build time improvement:** Measure before/after compilation time
- **Code complexity reduction:** Fewer modules to maintain
- **Clarity improvement:** No more `#![allow(dead_code)]` suppressions

---

## Related Issues

During this investigation, the following potential issues were identified:

1. **Missing module declarations:**
   - `code-rs/core/src/tasks/` is used but not declared
   - `code-rs/tui/src/custom_terminal.rs` is used in tests but not declared

2. **Broken test infrastructure:**
   - Unit tests in code-rs/tui have 11 compilation errors even without legacy_tests
   - May need separate cleanup effort for test infrastructure

3. **Upstream sync opportunity:**
   - codex-rs/tui has working vt100-tests
   - Consider whether to port these tests or maintain divergence

---

## Notes for Future Maintenance

1. **Avoid `#![allow(dead_code)]` at file level** - This suppresses important warnings and makes it hard to identify truly dead code. Use at item level only when justified.

2. **Remove code instead of commenting it out** - Git preserves history; commented-out module declarations (like `text_block.rs` in lib.rs) create confusion.

3. **Clean up stale comments** - Comments about "removed" code should be deleted along with the code.

4. **Keep feature flags clean** - Remove unused feature flags promptly to avoid confusion.

5. **Test infrastructure hygiene** - If tests don't compile, either fix them or delete them. Don't hide behind feature flags.

---

**End of Document**

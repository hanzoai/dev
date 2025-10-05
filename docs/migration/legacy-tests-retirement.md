# Legacy Tests Retirement Plan

**Status:** Draft Proposal
**Date:** 2025-10-05
**Scope:** `code-tui` crate legacy test suites behind `legacy_tests` feature flag

---

## Executive Summary

The `code-tui` crate contains **35+ individual test functions** across **7 active test modules** gated behind the `legacy_tests` feature flag. These tests cover critical functionality including text layout, markdown rendering, diff visualization, clipboard handling, and shell command processing. This document provides an inventory of these legacy tests, analyzes their current state, and proposes a retirement strategy.

**Key Findings:**
- **2 tests are broken** (reference removed code in `clipboard_paste.rs`)
- **1 test module reference is dead** (`chatwidget_stream_tests.rs` file doesn't exist)
- **415 active tests** exist in the codebase (found via grep across 45 files)
- Legacy tests provide coverage for mature, stable code paths

---

## Inventory of Legacy Test Suites

### 1. Text Layout & Wrapping Tests

#### File: `code-rs/tui/src/live_wrap.rs` (lines 204-285)
- **Tests:** 5 test functions
- **Functionality:** Word-wrapping and text layout for terminal display
- **Test Coverage:**
  - ASCII text wrapping respects target width
  - Wide characters (emoji, CJK) proper measurement and wrapping
  - Fragmentation invariance (same output regardless of input chunking)
  - Newline handling with explicit break flags
  - Dynamic width changes trigger rewrapping
- **Dependencies:** `RowBuilder` struct
- **Risk Level:** 🟡 Medium - Core text display logic
- **Replacement Coverage:** No modern equivalent tests identified

#### File: `code-rs/tui/src/insert_history.rs` (lines 550-630)
- **Tests:** 3 test functions
- **Functionality:** ANSI escape sequence generation and styled text wrapping
- **Test Coverage:**
  - Bold/normal ANSI transitions
  - Word-aware wrapping at specified widths
  - Style preservation across line boundaries
- **Dependencies:** Terminal ANSI output, scroll region manipulation
- **Risk Level:** 🟡 Medium - Terminal output correctness
- **Replacement Coverage:** No modern equivalent tests identified

---

### 2. Markdown Rendering Tests

#### File: `code-rs/tui/src/markdown.rs` (lines 395-646)
- **Tests:** 11 test functions
- **Functionality:** Markdown parsing and rendering with citation rewriting
- **Test Coverage:**
  - File citation rewriting to VS Code-style URIs (`vscode://file/...`)
  - Absolute vs. relative path resolution
  - Citation spacing prevention
  - Fenced code block whitespace preservation
  - Indented code block whitespace preservation
  - Citation protection inside code blocks
  - Blank line handling in code blocks
  - List item detection (prevent indent→code misinterpretation)
- **Dependencies:** Markdown parser, file path utilities
- **Risk Level:** 🟢 Low-Medium - Mature stable feature
- **Replacement Coverage:** No modern equivalent tests identified

---

### 3. Diff Visualization Tests

#### File: `code-rs/tui/src/diff_render.rs` (lines 664-819)
- **Tests:** 6 snapshot tests (using `insta` crate)
- **Functionality:** Diff and patch rendering
- **Test Coverage:**
  - File addition rendering with line counts
  - File update rendering with rename display
  - Long line wrapping in diff output
  - Single-line replacement counts
  - Blank context line rendering
  - Multi-hunk vertical ellipsis separation (⋮)
- **Dependencies:** `insta` snapshot testing, diff formatting
- **Risk Level:** 🟢 Low - UI/UX validation, easily manually verified
- **Replacement Coverage:** No modern equivalent tests identified

---

### 4. Shell Command Processing Tests

#### File: `code-rs/tui/src/exec_command.rs` (lines 68-85)
- **Tests:** 2 test functions
- **Functionality:** Shell command escaping and wrapper detection
- **Test Coverage:**
  - Shell escaping for spaces and special characters
  - `bash -lc` wrapper stripping
- **Dependencies:** String utilities, shell command formatting
- **Risk Level:** 🟡 Medium - Security-relevant (shell injection prevention)
- **Replacement Coverage:** No modern equivalent tests identified

---

### 5. Clipboard & Path Handling Tests

#### File: `code-rs/tui/src/clipboard_paste.rs` (lines 215-330)
- **Tests:** 11 test functions (2 BROKEN)
- **Functionality:** Clipboard paste normalization and path handling
- **Test Coverage:**
  - `file://` URL → filesystem path conversion (POSIX & Windows)
  - Shell-escaped path unescaping
  - Quote handling (single and double quotes)
  - Windows path detection (drive letters, UNC paths)
  - Multi-token rejection
  - **BROKEN:** `pasted_image_format_png_jpeg_unknown` (lines 263-285)
  - **BROKEN:** `pasted_image_format_with_windows_style_paths` (lines 315-329)
- **Dependencies:** Path normalization utilities
- **Risk Level:** 🔴 High - Contains broken tests, cross-platform path handling
- **Broken Tests Details:**
  - Reference `pasted_image_format()` function (doesn't exist)
  - Reference `EncodedImageFormat::{Jpeg, Other}` enum variants (removed)
- **Replacement Coverage:** Unknown

---

### 6. Widget Streaming & Retry Tests

#### File: `code-rs/tui/src/lib.rs` (lines 97-98)
- **Tests:** Module import only (file missing)
- **Functionality:** VT100-based replay tests for widget streaming
- **Test Coverage:** NONE - `chatwidget_stream_tests.rs` file does not exist
- **Dependencies:** N/A
- **Risk Level:** 🟢 Low - Dead code reference
- **Replacement Coverage:** N/A

#### File: `code-rs/tui/src/chatwidget.rs` (lines 76-77)
- **Tests:** Module import for `tests_retry`
- **Functionality:** Retry mechanisms in chat widget
- **Test Coverage:** (Tests exist but are NOT gated by `legacy_tests`)
  - The `tests_retry.rs` module exists but does NOT use `#[cfg(feature = "legacy_tests")]`
  - This is a false positive in the inventory - these are active tests
- **Dependencies:** Retry logic, backoff strategies
- **Risk Level:** 🟢 Low - Tests are active, not legacy
- **Replacement Coverage:** N/A (tests are current)

---

## Summary Statistics

| Metric | Count |
|--------|-------|
| Total files with `legacy_tests` references | 8 files |
| Active legacy test modules | 6 modules |
| Dead references | 2 (missing file + non-legacy module) |
| Total individual legacy test functions | ~35 tests |
| Broken tests | 2 tests |
| Active non-legacy tests in codebase | 415+ tests (across 45 files) |

---

## Risk Assessment

### High Risk (Requires Immediate Attention)
- **`clipboard_paste.rs`** - Contains 2 broken tests that won't compile with `--features legacy_tests`
  - Action: Fix or delete broken tests

### Medium Risk (Needs Replacement Coverage)
- **`live_wrap.rs`** - Core text layout logic, no replacement tests
- **`insert_history.rs`** - Terminal output correctness, no replacement tests
- **`exec_command.rs`** - Security-relevant shell escaping, no replacement tests

### Low Risk (Stable, Low-Touch Code)
- **`markdown.rs`** - Mature feature, manually verifiable
- **`diff_render.rs`** - UI/UX validation, snapshot tests

### No Risk (Can Delete Immediately)
- **`lib.rs`** reference to `chatwidget_stream_tests` - File doesn't exist
- **`chatwidget.rs`** reference - Tests are active (not legacy)

---

## Retirement Strategy

### Phase 1: Immediate Cleanup (Low Effort, High Value)
**Timeline:** 1-2 days
**Goal:** Remove dead code and fix broken tests

1. **Delete dead reference in `lib.rs:97-98`**
   - Remove `#[cfg(all(test, feature = "legacy_tests"))] mod chatwidget_stream_tests;`
   - File doesn't exist, safe to delete

2. **Fix or delete broken tests in `clipboard_paste.rs`**
   - **Option A:** Delete tests on lines 263-285 and 315-329 (recommended if functionality removed)
   - **Option B:** Investigate if `pasted_image_format` should be restored or tests updated

3. **Verify `chatwidget.rs` tests are not legacy**
   - Remove from legacy_tests inventory (false positive)
   - These tests should remain active

**Deliverables:**
- [ ] PR: Remove dead `chatwidget_stream_tests` reference
- [ ] PR: Fix/delete broken `clipboard_paste.rs` tests
- [ ] Updated inventory removing false positives

---

### Phase 2: Coverage Analysis (Medium Effort)
**Timeline:** 1 week
**Goal:** Determine which legacy tests have equivalent modern coverage

1. **Analyze modern test suite**
   - Run `cargo test -p code-tui --lib` to see active tests
   - Compare functionality coverage between legacy and active tests
   - Identify gaps in modern test coverage

2. **Interview developers**
   - Why were these tests moved to `legacy_tests`?
   - Are these code paths still actively maintained?
   - What modern testing approach replaced them?

3. **Create coverage matrix**
   - Map each legacy test to:
     - Modern replacement test (if exists)
     - Alternative validation method (manual QA, integration tests)
     - "No coverage" flag (requires new tests)

**Deliverables:**
- [ ] Coverage analysis report
- [ ] List of legacy tests with no modern equivalent
- [ ] List of legacy tests safe to retire

---

### Phase 3: Selective Retirement (High Effort)
**Timeline:** 2-4 weeks
**Goal:** Retire safe-to-delete tests, preserve or modernize critical tests

#### Retirement Candidates (Safe to Delete)
Based on Phase 2 analysis, likely candidates:

1. **Low-risk UI/UX tests**
   - `diff_render.rs` snapshot tests (if manually verified in QA)
   - `markdown.rs` citation tests (if feature is stable and rarely changes)

2. **Redundant tests**
   - Any legacy test with proven modern equivalent coverage

#### Preservation Candidates (Modernize or Keep)
High-value tests that should be preserved:

1. **Security-critical tests**
   - `exec_command.rs` shell escaping tests
   - Move to active test suite (remove `legacy_tests` gate)

2. **Core functionality with no replacement**
   - `live_wrap.rs` text layout tests
   - `insert_history.rs` ANSI output tests
   - Either modernize or move to active suite

3. **Cross-platform tests**
   - `clipboard_paste.rs` path normalization (after fixing broken tests)
   - Critical for Windows/Linux/macOS compatibility

#### Modernization Approach
For tests being preserved:

1. **Remove `legacy_tests` feature gate**
   - Tests run by default in CI

2. **Refactor to modern patterns**
   - Update to use newer testing utilities if needed
   - Improve test names/documentation

3. **Add integration tests**
   - Some unit tests may be better as integration tests

**Deliverables:**
- [ ] PRs retiring safe-to-delete legacy tests
- [ ] PRs modernizing preserved tests
- [ ] Updated CI configuration
- [ ] Documentation of retired vs. preserved tests

---

### Phase 4: Feature Flag Removal (Final Step)
**Timeline:** 1 day
**Goal:** Remove `legacy_tests` feature entirely

Once all legacy tests are retired or modernized:

1. **Remove feature flag from `Cargo.toml`**
   - Delete line 20: `legacy_tests = []`

2. **Clean up CI scripts**
   - Remove any `--features legacy_tests` invocations

3. **Update documentation**
   - Archive this retirement plan
   - Document migration in changelog

**Deliverables:**
- [ ] PR: Remove `legacy_tests` feature flag
- [ ] Updated CHANGELOG.md
- [ ] Archived retirement plan in `docs/archive/`

---

## Sequencing Recommendations

### Recommended Order
1. **Phase 1** (Immediate Cleanup) - Do first, unblocks everything else
2. **Phase 2** (Coverage Analysis) - Required before making retirement decisions
3. **Phase 3** (Selective Retirement) - Based on Phase 2 findings
4. **Phase 4** (Feature Flag Removal) - Final cleanup

### Parallel Work Opportunities
- Phase 1 can happen immediately while planning Phase 2
- Individual test file retirements in Phase 3 can be done in parallel PRs

### Critical Path
- **Must fix `clipboard_paste.rs` broken tests** before running any `--features legacy_tests` builds
- **Must complete coverage analysis** before retiring any medium/high-risk tests

---

## Required Follow-up Work

### Development Work
1. Fix or delete broken tests in `clipboard_paste.rs`
2. Investigate why `pasted_image_format` was removed
3. Run full test suite with `--features legacy_tests` to identify any other compilation failures
4. Document rationale for each test retirement decision

### Documentation Work
1. Update testing guidelines to prevent future "legacy test" accumulation
2. Document modern testing patterns to replace legacy approaches
3. Create runbook for test retirement process

### Process Improvements
1. Establish policy: New tests must NOT use `legacy_tests` feature
2. Set deadline for legacy test retirement (e.g., end of Q1 2026)
3. Add CI check to fail if new `#[cfg(feature = "legacy_tests")]` added

---

## Open Questions

1. **Why were these tests moved to `legacy_tests` originally?**
   - Historical context needed from team

2. **Are there modern equivalents we haven't found?**
   - Need comprehensive test coverage report

3. **Should any legacy tests become integration tests instead?**
   - Some unit tests may be better suited as E2E tests

4. **What is the current test coverage percentage?**
   - Baseline metric needed before retirement

5. **Are these code paths actively maintained?**
   - If code is deprecated, tests can be deleted
   - If code is active, tests should be preserved/modernized

---

## Appendix A: Test Inventory Details

### Complete File-by-File Breakdown

#### 1. `live_wrap.rs` Legacy Tests
```rust
#[cfg(all(test, feature = "legacy_tests"))]
mod tests {
    #[test] fn rows_do_not_exceed_width_ascii()
    #[test] fn rows_do_not_exceed_width_emoji_cjk()
    #[test] fn fragmentation_invariance_long_token()
    #[test] fn newline_splits_rows()
    #[test] fn rewrap_on_width_change()
}
```

#### 2. `insert_history.rs` Legacy Tests
```rust
#[cfg(all(test, feature = "legacy_tests"))]
mod tests {
    #[test] fn writes_bold_then_regular_spans()
    #[test] fn word_wrap_line_simple()
    #[test] fn word_wrap_line_preserves_styles()
}
```

#### 3. `markdown.rs` Legacy Tests
```rust
#[cfg(all(test, feature = "legacy_tests"))]
mod tests {
    #[test] fn citation_is_rewritten_with_absolute_path()
    #[test] fn citation_is_rewritten_with_relative_path()
    #[test] fn citation_followed_by_space_so_they_do_not_run_together()
    #[test] fn citation_unchanged_without_file_opener()
    #[test] fn fenced_code_blocks_preserve_leading_whitespace()
    #[test] fn citations_not_rewritten_inside_code_blocks()
    #[test] fn indented_code_blocks_preserve_leading_whitespace()
    #[test] fn citations_not_rewritten_inside_indented_code_blocks()
    #[test] fn append_markdown_preserves_full_text_line()
    #[test] fn fenced_code_block_with_internal_blank_line_is_one_contiguous_block()
    #[test] fn nested_list_items_not_treated_as_code_blocks()
}
```

#### 4. `diff_render.rs` Legacy Tests
```rust
#[cfg(all(test, feature = "legacy_tests"))]
mod tests {
    #[test] fn ui_snapshot_add_details()
    #[test] fn ui_snapshot_update_details_with_rename()
    #[test] fn ui_snapshot_wrap_behavior_insert()
    #[test] fn ui_snapshot_single_line_replacement_counts()
    #[test] fn ui_snapshot_blank_context_line()
    #[test] fn ui_snapshot_vertical_ellipsis_between_hunks()
}
```

#### 5. `exec_command.rs` Legacy Tests
```rust
#[cfg(all(test, feature = "legacy_tests"))]
mod tests {
    #[test] fn test_escape_command()
    #[test] fn test_strip_bash_lc_and_escape()
}
```

#### 6. `clipboard_paste.rs` Legacy Tests (2 BROKEN)
```rust
#[cfg(all(test, feature = "legacy_tests"))]
mod pasted_paths_tests {
    #[test] fn normalize_file_url()
    #[test] fn normalize_file_url_windows()
    #[test] fn normalize_shell_escaped_single_path()
    #[test] fn normalize_simple_quoted_path_fallback()
    #[test] fn normalize_single_quoted_unix_path()
    #[test] fn normalize_multiple_tokens_returns_none()
    #[test] fn pasted_image_format_png_jpeg_unknown() // ❌ BROKEN
    #[test] fn normalize_single_quoted_windows_path()
    #[test] fn normalize_unquoted_windows_path_with_spaces()
    #[test] fn normalize_unc_windows_path()
    #[test] fn pasted_image_format_with_windows_style_paths() // ❌ BROKEN
}
```

---

## Appendix B: Commands for Analysis

### Run legacy tests
```bash
cargo test -p code-tui --lib --features legacy_tests
```

### Run current tests (no legacy)
```bash
cargo test -p code-tui --lib
```

### Count test functions
```bash
rg '#\[test\]' code-rs/tui/src/ | wc -l
```

### Find all legacy_tests references
```bash
rg 'legacy_tests' code-rs/tui/
```

### Check for broken tests
```bash
cargo test -p code-tui --lib --features legacy_tests 2>&1 | grep -i error
```

---

## Revision History

| Date | Version | Author | Changes |
|------|---------|--------|---------|
| 2025-10-05 | 1.0 | Claude (Agent) | Initial draft proposal |


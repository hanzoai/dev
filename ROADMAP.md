# Roadmap

## Objectives
- Protect our ability to absorb upstream changes quickly while letting `code-rs` own the fork-only UX, policy, and tooling layers.
- Remove legacy compatibility scaffolding (dead modules, feature flags, tests, assets) so the tree matches the product we actually ship.
- Rebuild the test suite around lightweight smoke/targeted coverage that reflects `code-rs` behavior today instead of the old codex execution model.
- Institutionalize upstream tracking, reuse decisions, and maintenance checklists so each merge is predictable.
- Standardize parallel-agent workflows (one worktree per track) to keep large refactors moving without blocking on long-running investigations.

## Upstream Reuse Strategy
| Area | Current Delta | Strategy | Status & Actions |
| --- | --- | --- | --- |
| `mcp-client` | Rename + larger stdout buffer. | Adopt upstream crate directly, keep fork-only buffer change via wrapper if still required. | ✅ **Completed (2025-10-05)**: Re-exported upstream `codex-mcp-client` as `code-mcp-client` with thin wrapper preserving buffer settings. |
| `responses-api-proxy` | Rename + larger API key buffer + extra test. | Consume upstream crate, keep buffer tweak behind feature or downstream patch. | ✅ **Completed (2025-10-05)**: Re-exported upstream `codex-responses-api-proxy` as `code-responses-api-proxy` with minimal fork-specific changes. |
| `exec` | Substantial policy/output changes. | Build adapters for upstream executor where possible; retain fork-specific approval pipeline. | Catalogue fork-only code paths (policy prompts, streaming) and design adapter boundaries in `code-core`. |
| `login` | Auth flow tweaks, HTML assets, approval prompts. | Keep forked implementation, but cross-check with upstream helpers for OAuth updates. | Diff upstream releases regularly; cherry-pick auth fixes while preserving fork UX. |
| `protocol` | Added MCP protocol types. | Maintain fork version; push reusable schema changes upstream if possible. | Document any upstream schema additions needed for fork behavior. |
| `core`, `mcp-server`, `tui`, `app-server` | Massive divergence (fork-only features). | Treat as fork-owned; only pull upstream when changes unblock us. | Record upstream changes that impact prompts/executor and implement targeted integrations. |

## Codebase Cleanup Plan
Supporting detail: `DEAD_CODE_INVENTORY.md`

### Completed (as of 2025-10-05)
- ✅ Removed legacy test suite (`legacy_tests` feature and old codex execution tests)
  - Deleted ~106 test files across 8 crates (app-server, apply-patch, chatgpt, cli, core, exec, login, mcp-server)
  - Removed ~35,000 lines of outdated test code
  - See `docs/migration/legacy-tests-retirement.md` for porting plan
- ✅ Removed legacy TUI test harness files (`chatwidget/tests.rs`, `chatwidget/tests_retry.rs`) and the orphaned `exec_cell/render.rs`
- ✅ Removed defunct TUI overlay/backtrack modules (`pager_overlay.rs`, `backtrack_helpers.rs`, `resume_picker.rs`)
- ✅ Archived `code-rs/tui/agent_tasks/` planning docs under `docs/archive/tui-migrations/`
- ✅ Deleted `code-rs/tui/src/compat.rs` ratatui adapter layer; import ratatui types directly
- ✅ Removed all `cfg(feature = "legacy_tests")` blocks
- ✅ Removed `vt100-tests` feature flag from `code-rs/tui/Cargo.toml` (2025-10-05)
- ✅ Created comprehensive migration documentation in `docs/migration/` and `docs/maintenance/`

### Needs Follow-Up
- ⏳ Audit remaining TUI modules marked `#![allow(dead_code)]` (streaming controller, markdown stack)
- ⏳ Reconcile vt100 rendering coverage — upstream snapshots live only in `codex-rs`

### Active Work
1. **Prune unused modules and assets**
   - 📋 Audit `code-rs/tui` for modules ported from upstream but unused (e.g., status widgets, legacy assets).
   - 📋 Review `code-rs/core` for upstream executor skeletons left over from earlier merges (`tools/`, `executor/`, etc.) and remove or quarantine them.
2. **Consolidate prompts and config**
   - ✅ Audited prompt files; snapshot recorded in `docs/maintenance/prompt-architecture.md`; no unused prompts detected.
   - 📋 Monitor prompt changes during upstream merges per `docs/maintenance/upstream-diff.md`
3. **Simplify feature flags and cfgs**
   - 📋 Audit remaining fork-only flags (e.g., `code-fork`) and `code-*` prefix consistency.

## Test Suite Reset
Supporting detail: `TEST_SUITE_RESET.md`, `docs/migration/legacy-tests-retirement.md`

### Phase 1: Completed (2025-10-05)
- ✅ Removed `legacy_tests` feature and deleted unit tests targeting the old codex execution flow (~106 files, 35,000+ lines).
- ✅ Dropped vt100 fixtures and binary-size snapshots that no longer reflect current UI behavior.
- ✅ Verified TUI smoke tests passing (all bottom panes, approval flow, markdown/code streaming, auto-drive sessions).
- ✅ Build verification passed with no regressions.

### Phase 2-3: In Progress (Oct 20 - Nov 30, 2025)
**Focus:** Port critical test coverage and expand smoke tests.

**Known Blockers (9 legacy-only test files requiring replacement):**
- `codex-rs/core/tests/suite/`: `model_tools.rs`, `tool_harness.rs`, `tools.rs`, `read_file.rs`, `view_image.rs`, `unified_exec.rs` (6 files)
- `codex-rs/tui/tests/suite/`: `vt100_history.rs`, `vt100_live_commit.rs`, `status_indicator.rs` (3 files)

**Timeline & Actions:**
1. **Phase 2 (Oct 20-31)**: Core Runtime Tests
   - 📋 Port tool execution tests to `code-rs/core/tests/suite/`
   - 📋 Port unified executor tests (stdin session reuse, timeout handling)
   - 📋 Run smoke tests and verify no regressions

2. **Phase 3 (Nov 1-15)**: TUI Rendering Tests
   - 📋 Create `code-rs/tui/tests/` directory structure
   - 📋 Port VT100 rendering tests (7+ test cases for wrapping/ANSI/emoji/CJK)
   - 📋 Add golden file fixtures for visual regression testing

3. **Phase 4 (Nov 16-30)**: Final Cleanup
   - 📋 Audit coverage comparison, confirm no orphaned tests
   - 📋 Remove upstream test directories if appropriate
   - 📋 Archive playbook to `docs/archive/`

**Current Test Scaffolding:**
- ✅ Added base scenarios in `tui/tests/ui_smoke.rs`
- ✅ Added `chatwidget::smoke_helpers` with `ChatWidgetHarness` and assertion helpers
- 📋 Extract similar helpers for `App` and executor contexts
- 📋 Strengthen assertions for exec/approval/tool flows

See `docs/migration/legacy-tests-retirement.md` for detailed checklist, risk assessment, and success criteria.

## Upstream Tracking Process
- **Monthly cadence**: First Monday = quick diff, Second Monday = merge planning (see `docs/maintenance/upstream-diff.md`).
- Use the scripts under `scripts/upstream-merge/` (`diff-crates.sh`, `highlight-critical-changes.sh`, `log-merge.sh`) as described in `docs/maintenance/upstream-diff.md`.
- Monitor `codex-rs/core/prompt.md`, executor/plan tool updates, and API schema changes during each upstream merge.
- Maintain a CHANGELOG section summarizing upstream features adopted vs intentionally skipped.
- For reusable crates we adopt, pin the upstream commit hash and bump deliberately (documenting any fork-specific patches).

## Execution Phases
1. **Baseline & Tooling** ✅ *Complete (2025-10-05)*
   - ✅ Reverted/deleted unused compatibility modules (compat.rs, app_backtrack.rs, etc.)
   - ✅ Installed upstream diff scripts; verified they produce baseline reports (`scripts/upstream-merge/`)
   - ✅ Established parallel worktree workflow documentation
   - ✅ Created comprehensive docs in `docs/migration/` and `docs/maintenance/`

2. **Crate Adoption Pass** ✅ *Complete (2025-10-05)*
   - ✅ Adopted `codex-mcp-client` and `codex-responses-api-proxy` with thin wrappers
   - ✅ Documented wrapper decisions and pinned versions in `UPSTREAM_PARITY_CHECKLIST.md`
   - ✅ Verified build passes with upstream crate dependencies

3. **Fork Cleanup Pass** ✅ *Complete (2025-10-05)*
   - ✅ Executed P0/P1 deletions from `DEAD_CODE_INVENTORY.md` (106 test files, ~35K lines)
   - ✅ Archived outdated planning docs under `docs/archive/tui-migrations/`
   - ✅ Removed `legacy_tests` feature flag and all associated test infrastructure
   - 📋 Remaining: Audit `code-fork` feature usage and unused modules in `code-rs/core`

4. **Test Suite Rebuild** 🔄 *Phase 1 Complete, Phase 2-4 In Progress (Oct 20 - Nov 30)*
   - ✅ Phase 1: Removed legacy test suites, verified smoke tests pass, build verification clean
   - 📋 Phase 2 (Oct 20-31): Port 6 critical core runtime tests
   - 📋 Phase 3 (Nov 1-15): Port 3 TUI rendering tests, add golden file fixtures
   - 📋 Phase 4 (Nov 16-30): Final coverage audit and cleanup
   - See `docs/migration/legacy-tests-retirement.md` for detailed plan

5. **Upstream Tracking & Maintenance** 🔄 *Ongoing*
   - ✅ Established monthly cadence (First Monday: diff, Second Monday: merge planning)
   - ✅ Created tracking scripts and logging infrastructure (`scripts/upstream-merge/`)
   - 📋 Schedule first upstream diff review for next month
   - 📋 Capture learnings from each parallel-agent run to refine workflow

## Next Actions (Priority Order)

### Completed (2025-10-05) ✅
1. ✅ **Audit remaining fork-specific code**
   - ✅ Reviewed `code-fork` feature flag usage - properly used, gates 12 fork-specific TUI extensions
   - ✅ Identified module usage in `code-rs/core` - all modules (`codex/`, `unified_exec/`, `exec_command/`) actively used
   - ✅ Documented findings in `DEAD_CODE_INVENTORY.md` with comprehensive audit summary

2. ✅ **Document test scaffolding patterns**
   - ✅ Documented ChatWidgetHarness and assertion helpers in `TEST_SUITE_RESET.md`
   - ✅ Added usage examples, best practices, and extension guide
   - ✅ Identified future test infrastructure needs (AppHarness, ExecutorHarness, MCPHarness)

### Active Work
1. **Begin Phase 2: Core Runtime Test Porting (Oct 20-31)**
   - Port 6 critical test files from `codex-rs/core/tests/suite/` to `code-rs/core/tests/suite/`
   - Focus: `model_tools.rs`, `tool_harness.rs`, `tools.rs`, `read_file.rs`, `view_image.rs`, `unified_exec.rs`
   - See detailed checklist in `docs/migration/legacy-tests-retirement.md`
   - **Blocker:** Requires `codex-rs/` directory (upstream checkout)

2. **Schedule upstream diff review**
   - Run `scripts/upstream-merge/diff-crates.sh --all` on first Monday of month
   - Review `CRITICAL-SUMMARY.md` for changes requiring attention
   - Log decisions per `docs/maintenance/upstream-diff.md` workflow
   - **Prerequisite:** Checkout upstream to `codex-rs/` directory

3. **Expand smoke test coverage** (ongoing)
   - Strengthen assertions in `tui/tests/ui_smoke.rs` for exec/approval/tool flows
   - Add executor and MCP integration test helpers following documented patterns
   - Expand `smoke_helpers.rs` with additional assertion helpers as needed

## Success Metrics
Track progress against these deliverables:

| Metric | Baseline (2025-10-05) | Target | Status |
|--------|----------------------|--------|--------|
| **Codebase Size** | 35K lines removed | Clean tree, fork-only modules | ✅ Complete |
| **Test Files** | 106 legacy files deleted | 9 critical tests ported | 🔄 0/9 ported |
| **Test Coverage** | ~85% (estimated) | ≥90% with new suite | 📋 TBD |
| **Build Time** | 1m 30s (dev-fast) | ≤ baseline | ✅ No regression |
| **Upstream Crate Adoption** | 2/2 crates (mcp-client, responses-api-proxy) | Wrapper overhead minimal | ✅ Complete |
| **Documentation** | Migration docs created | Complete guides in docs/ | ✅ Complete |
| **Upstream Tracking** | Scripts installed | Monthly cadence active | 📋 First review pending |

## Deliverables
- ✅ Clean `code-rs` tree with only fork-relevant modules (Phase 1-3 complete)
- 🔄 Minimal but working test suite aligned with current behavior (Phase 2-4 in progress, 9 tests to port)
- ✅ Clearly documented integration points for upstream crates and prompts
- ✅ Migration playbooks and maintenance guides in `docs/migration/` and `docs/maintenance/`
- 📋 Monthly upstream tracking cadence established and operational

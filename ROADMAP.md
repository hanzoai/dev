# Roadmap

## Objectives
- Protect our ability to absorb upstream changes quickly while letting `code-rs` own the fork-only UX, policy, and tooling layers.
- Remove legacy compatibility scaffolding (dead modules, feature flags, tests, assets) so the tree matches the product we actually ship.
- Rebuild the test suite around lightweight smoke/targeted coverage that reflects `code-rs` behavior today instead of the old codex execution model.
- Institutionalize upstream tracking, reuse decisions, and maintenance checklists so each merge is predictable.
- Standardize parallel-agent workflows (one worktree per track) to keep large refactors moving without blocking on long-running investigations.

## Upstream Reuse Strategy
| Area | Current Delta | Strategy | Immediate Actions |
| --- | --- | --- | --- |
| `mcp-client` | Rename + larger stdout buffer. | Adopt upstream crate directly, keep fork-only buffer change via wrapper if still required. | See `INVESTIGATION_SUMMARY.md` for options; test whether the 1 MB buffer is needed, then re-export upstream `codex-mcp-client` with a thin CLI shim if so. |
| `responses-api-proxy` | Rename + larger API key buffer + extra test. | Consume upstream crate, keep buffer tweak behind feature or downstream patch. | Follow `docs/upstream-mcp-reuse-strategy.md` to rebase local buffer changes into a minimal patch and depend on upstream. |
| `exec` | Substantial policy/output changes. | Build adapters for upstream executor where possible; retain fork-specific approval pipeline. | Catalogue fork-only code paths (policy prompts, streaming) and design adapter boundaries in `code-core`. |
| `login` | Auth flow tweaks, HTML assets, approval prompts. | Keep forked implementation, but cross-check with upstream helpers for OAuth updates. | Diff upstream releases regularly; cherry-pick auth fixes while preserving fork UX. |
| `protocol` | Added MCP protocol types. | Maintain fork version; push reusable schema changes upstream if possible. | Document any upstream schema additions needed for fork behavior. |
| `core`, `mcp-server`, `tui`, `app-server` | Massive divergence (fork-only features). | Treat as fork-owned; only pull upstream when changes unblock us. | Record upstream changes that impact prompts/executor and implement targeted integrations. |

## Codebase Cleanup Plan
Supporting detail: `DEAD_CODE_INVENTORY.md`

1. **Remove stale compat shims**
   - Delete `code-rs/tui/src/compat.rs`, `foundation.rs`, and other adapter modules introduced for abandoned compatibility work.
   - Strip feature flags such as `code-fork` and `legacy_tests` where they no longer gate real behavior.
2. **Prune unused modules and assets**
   - Audit `code-rs/tui` for modules ported from upstream but unused (e.g., status widgets, legacy assets).
   - Review `code-rs/core` for upstream executor skeletons left over from earlier merges (`tools/`, `executor/`, etc.) and remove or quarantine them.
3. **Consolidate prompts and config**
   - Keep only fork-specific prompt files (e.g., `prompt_coder.md`); archive or delete upstream variants that are no longer consumed.
4. **Simplify feature flags and cfgs**
   - Replace `cfg(feature = "legacy_tests")` blocks with explicit test modules or drop them entirely.
   - Ensure crate names and binaries use the `code-*` prefix consistently.

## Test Suite Reset
Supporting detail: `TEST_SUITE_RESET.md`

1. **Retire legacy suites**
   - Remove `legacy_tests` feature and delete unit tests that target the old codex execution flow.
   - Drop vt100 fixtures and binary-size snapshots that no longer reflect current UI behavior.
2. **Seed minimal, reliable coverage**
   - Keep smoke-style tests (e.g., new `tests/ui_smoke.rs`) that exercise critical flows without upstream dependencies.
   - Add targeted executor and MCP tests that validate current fork behavior.
3. **Create new test scaffolding**
   - Introduce helper modules for building `ChatWidget`, `App` instances, and executor contexts tailored to `code-rs`.
   - Document how to add new tests; enforce `./build-fast.sh --workspace code` as the only required gate.

## Upstream Tracking Process
- Use the scripts under `scripts/upstream-merge/` (`diff-crates.sh`, `highlight-critical-changes.sh`, `log-merge.sh`) as described in `docs/maintenance/upstream-diff.md`.
- Monitor `codex-rs/core/prompt.md`, executor/plan tool updates, and API schema changes during each upstream merge.
- Maintain a CHANGELOG section summarizing upstream features adopted vs intentionally skipped.
- For reusable crates we adopt, pin the upstream commit hash and bump deliberately (documenting any fork-specific patches).

## Execution Phases (No Dates)
1. **Baseline & Tooling**
   - Revert or delete unused compatibility modules and features (see `DEAD_CODE_INVENTORY.md`).
   - Install upstream diff scripts; verify they run and produce baseline reports (`scripts/upstream-merge/`).
   - Establish parallel worktree workflow (one agent per track, summarized back into ROADMAP).
2. **Crate Adoption Pass**
   - Trial direct dependencies on `codex-mcp-client` / `codex-responses-api-proxy` (guided by `INVESTIGATION_SUMMARY.md`).
   - Document wrapper decisions and pin versions in `UPSTREAM_PARITY_CHECKLIST.md`.
3. **Fork Cleanup Pass**
   - Execute P0/P1 deletions from `DEAD_CODE_INVENTORY.md` (feature flags, orphan modules, stale comments).
   - Archive or delete outdated planning docs that referenced abandoned compat efforts.
4. **Test Suite Rebuild**
   - Remove legacy suites per `TEST_SUITE_RESET.md`; preserve only smoke/critical tests.
   - Add new helper modules so future tests can be written without reintroducing upstream assumptions.
5. **Upstream Tracking & Maintenance**
   - For each upstream merge, run diff scripts, log decisions with `log-merge.sh`, and update ROADMAP sections.
   - Capture learnings from each parallel-agent run (what worked, what stalled) so future phases stay efficient.

## Deliverables
- Clean `code-rs` tree with only fork-relevant modules.
- Minimal but working test suite aligned with current behavior.
- Clearly documented integration points for upstream crates and prompts.
- ROADMAP updates as tasks complete; old planning documents removed.

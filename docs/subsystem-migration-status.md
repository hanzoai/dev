# Subsystem Migration Status (2025-10-05)

This tracker documents how each major area of the fork compares against the
fresh upstream snapshot in `codex-rs/` and whether we plan to reuse the upstream
implementation or keep a forked variant living under `code-rs/`.

| Subsystem | Upstream delta summary | Decision | Immediate next steps |
| --- | --- | --- | --- |
| Core runtime (`core/`, `exec/`, `protocol/`, `apply-patch/`) | Upstream added a unified `executor` module, tool router, and richer MCP hooks. Fork keeps pre-refactor pipeline plus fork-only policy/approval features. | **Hybrid** — adopt upstream executor/tool router incrementally while preserving fork-only approval flow. | Diff `code-rs/core` vs `codex-rs/core` focusing on `executor/*` and `tools/*`; design an adapter so forked approval + sandbox policy can plug into the upstream router without copy-pasting. |
| TUI (`tui/`) | Upstream reorganised history cells into typed renderers, added status dashboards/tests, and removed the older per-cell module tree. Fork still uses the legacy layout with Code-specific UX. | **Fork-primary** — keep `code-rs/tui` as the shipping implementation; cherry-pick upstream status widgets/tests once we add equivalent APIs in `code-rs`. | ✅ History/approval/event facade adapters landed behind `code-fork`. Next: finish routing remaining direct imports through compat modules, run manual smoke checklist, and continue pruning legacy test suites once replacements exist. |
| CLI (`cli/`, `login/`, `mcp-*`, `responses-api-proxy/`) | Upstream refreshed auth flows (OAuth helpers in `rmcp-client`) and expanded MCP tooling; fork adds policy prompts, approval gating. | **Hybrid** — reuse upstream auth/mcp client improvements by wiring them behind feature flags in `code-rs` while keeping fork-only prompts. | Prototype re-export of `codex-rs/rmcp-client` login helpers inside `code-rs/login`; confirm CLI flag parity before deleting fork copies. |
| Browser tooling (`browser/`, TUI browser events) | Upstream still lacks the browser agent; fork-only feature. | **Fork-only** — retain `code-rs/browser` and related TUI integrations. | Document browser integration in `docs/fork-enhancements.md` (already tracked); no upstream merge needed. |
| App server (`app-server`, `app-server-protocol`) | Upstream rewrote message processor to use new executor/tool traits. Fork still references old pipeline. | **Monitor** — do not adopt until core executor bridge (above) lands; otherwise duplicate effort. | Once the core adapter is ready, revisit to see if we can reuse upstream app-server entirely. |
| SDK / tooling (`sdk/typescript`, `scripts/*`) | Upstream scripts now assume `codex-*` bins. Fork updated to `code-*`. | **Fork-primary** with compatibility fallbacks. | Ensure scripts that read templates (e.g., `test-responses.js`) load both paths (already updated). Review future upstream changes quarterly. |
| Build & release tooling (`build-fast.sh`, nix flakes) | Upstream still targets a single workspace. Fork must handle both. | **Fork-only** — maintain dual-workspace script. | Keep script in sync with upstream bugfixes manually; add regression tests later. |

Last updated **2025-10-04**. Update this table whenever a subsystem decision
changes or a milestone completes.

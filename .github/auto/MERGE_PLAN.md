# Upstream Merge Plan (by-bucket)

Mode: by-bucket (MERGE_MODE=by-bucket)
Branch: upstream-merge
Upstream: openai/codex@main

Strategy
- Bucket 1 – Prefer Ours: `codex-rs/tui/**`, `codex-cli/**`, `.github/workflows/**`, `docs/**`, `AGENTS.md`, `README.md`, `CHANGELOG.md`.
  - Keep our UX, streaming order guarantees, and workflows intact.
- Bucket 2 – Prefer Theirs: `codex-rs/core/**`, `codex-rs/common/**`, `codex-rs/protocol/**`, `codex-rs/exec/**`, `codex-rs/file-search/**`.
  - Adopt upstream for correctness and protocol parity; adjust minimally to preserve our public API guarantees.
- Bucket 3 – Default: Adopt upstream outside Bucket 1 unless it conflicts with our tooling or breaks build/tests.
- Purge: Ensure images reintroduced under `.github/codex-cli-*.{png,jpg,jpeg,webp}` remain deleted.

Guardrails
- Preserve `codex-core` public re-exports: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream` and keep `codex_core::models` alias.
- Keep ICU/sys-locale deps unless proven unused.
- Do not reintroduce crates/paths listed as removed on default.

Process
1) Merge upstream/main without commit.
2) Enforce selection: ours for Bucket 1, theirs for Bucket 2.
3) Purge assets per policy.
4) Verify with `scripts/upstream-merge/verify.sh`; fix minimally.
5) Commit with concise message and summary; push branch and open PR.

Notes from artifacts
- Upstream touches: core rollout items, protocol changes, mcp user-agent suffix, cleanup of header env var, and TUI key handling. We will absorb core/protocol/mcp changes, retain our TUI.

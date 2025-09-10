# Upstream Merge Plan (by-bucket)

- Mode: by-bucket (grouped by areas from CHANGE_HISTOGRAM)
- Upstream: openai/codex@main → Branch: upstream-merge (existing)
- Policy:
  - Prefer ours: `codex-rs/tui/**`, `codex-cli/**`, `.github/workflows/**`, `docs/**`, `AGENTS.md`, `README.md`, `CHANGELOG.md`.
  - Prefer theirs: `codex-rs/core/**`, `codex-rs/common/**`, `codex-rs/protocol/**`, `codex-rs/exec/**`, `codex-rs/file-search/**`.
  - Default to upstream elsewhere unless it conflicts with build/UX.
  - Purge: image assets matching `.github/codex-cli-*.{png,jpg,jpeg,webp}`.

## Buckets

1) Core + Protocol (adopt upstream)
- Incorporate upstream correctness/security fixes in `codex-rs/core`, `mcp-*`, `protocol-*`.
- Keep public re-exports in `codex-core` as required.
- Maintain API compatibility inside our workspace when upstream surface changes.

2) TUI + CLI (keep ours)
- Preserve our unique TUI and CLI approach. Review upstream changes for compatible improvements, but default to ours.

3) Workflows/Docs (keep ours)
- Retain our workflows and documentation unless upstream changes are purely additive and non-breaking.

4) Cleanup
- Ensure purge globs remain deleted if reintroduced.

## Notes from artifacts
- Histogram: tui=146, core=77, docs=18, tests=11, other=102 → expect many TUI conflicts; we will keep ours there.
- Reintroduced paths include workflow files and images — keep ours; purge codex-cli images.
- Upstream added MCP user-agent handling; we’ll adopt in core/mcp while keeping internal call sites compatible.

## Verification
- Run `scripts/upstream-merge/verify.sh` and fix minimally until green.
- Final check with `./build-fast.sh` (no warnings).

## Commit & Push
- Single merge commit on `upstream-merge` with concise report.

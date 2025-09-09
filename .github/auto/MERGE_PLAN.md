# Upstream Merge Plan

- Mode: by-bucket (artifacts present; large delta across Rust workspace)
- Upstream: `openai/codex` @ `main`
- Branch: `upstream-merge` (existing; do not recreate)

## Strategy

- Prefer ours: `codex-rs/tui/**`, `codex-cli/**`, `.github/workflows/**`, `docs/**`, `AGENTS.md`, `README.md`, `CHANGELOG.md`.
- Prefer theirs: `codex-rs/core/**`, `codex-rs/common/**`, `codex-rs/protocol/**`, `codex-rs/exec/**`, `codex-rs/file-search/**`.
- Default elsewhere: adopt upstream unless it conflicts with our UX/tooling or breaks build.
- Purge if reintroduced: `.github/codex-cli-*.{png,jpg,jpeg,webp}`.

## Buckets

- Core/Protocol/Exec/Common/File-search: adopt upstream changes; keep public re-exports in `codex-core` and `codex_core::models` alias.
- TUI: keep our implementation; review upstream TUI diffs for any clearly beneficial, compatible fixes. Otherwise, resolve conflicts in favor of ours.
- Tooling/Workflows/Docs: keep ours to preserve fork identity unless upstream contains critical correctness/security fixes.

## Notable Upstream Themes (from artifacts)

- Protocol additions (ArchiveConversation, rollout_path), core client config var rename, apply_patch messaging tweak.
- TUI refactors and new tests/fixtures; we will avoid wholesale adoption to protect our UI approach.

## Validation

- Run `scripts/upstream-merge/verify.sh` (calls `./build-fast.sh` and compiles `codex-core` API tests).
- Treat warnings as failures; fix minimally within merge context.

## Reporting

- Record notable accepts/drops and any manual resolutions in `.github/auto/MERGE_REPORT.md`.

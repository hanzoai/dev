# Upstream Merge Plan

- Mode: by-bucket (per artifacts + policy)
- Branches:
  - Default: `main`
  - Merge target: `upstream-merge`
  - Upstream source: `upstream/main`

## Strategy

We will perform a single `--no-commit` merge of `upstream/main` into `upstream-merge`, then resolve conflicts bucket‑wise according to fork policy:

- Prefer ours: `codex-rs/tui/**`, `codex-cli/**`, and core integration files that gate or expose tools and version/UA semantics: `codex-rs/core/src/{openai_tools.rs,codex.rs,agent_tool.rs,default_client.rs}`, `codex-rs/protocol/src/models.rs`, `.github/workflows/**`, `docs/**`, `AGENTS.md`, `README.md`, `CHANGELOG.md`.
- Prefer theirs: foundational crates with low UX coupling: `codex-rs/common/**`, `codex-rs/exec/**`, `codex-rs/file-search/**`—unless it breaks our build or documented behavior.
- Purge any upstream reintroduced assets matching: `.github/codex-cli-*.(png|jpg|jpeg|webp)`.

## Invariants to Preserve

- Tool families and gating: all `browser_*`, `agent_*`, and `web_fetch` handlers must have matching tool schemas; preserve exposure gating logic.
- Screenshot UX: keep screenshot queuing/turn‑boundary semantics and TUI rendering behavior.
- Version/UA: continue using `codex_version::version()` and `get_codex_user_agent_default()` where applicable.
- Public re‑exports in `codex-core`: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`; keep `codex_core::models` alias.
- Do not remove ICU/sys‑locale if used elsewhere.

## Validation

- Run `scripts/upstream-merge/verify.sh` after conflict resolution.
- Build gate: `./build-fast.sh` must pass with zero warnings.

## Reporting

- Summarize notable accept/reject decisions and any purged files in `.github/auto/MERGE_REPORT.md`.


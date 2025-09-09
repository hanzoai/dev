# Upstream Merge Plan

Mode: by-bucket (per repo policy)

Strategy
- Prefer ours: codex-rs/tui, codex-cli, workflows, docs (branding/UX).
- Prefer theirs: codex-rs/{core,common,protocol,exec,file-search} for correctness and API parity.
- Default outside buckets: adopt upstream unless it conflicts with our UX or breaks build.
- Purge: ensure any reintroduced .github/codex-cli-*.{png,jpg,jpeg,webp} are removed.

Inputs
- Upstream: openai/codex@main
- Current branch: upstream-merge
- Artifacts:
- COMMITS.json: 3 commits (ArchiveConversation; rollout_path fix; CI speedup).
  - CHANGE_HISTOGRAM: tui-heavy changes; core/protocol touched.
  - DELTA_FILES/DIFFSTAT: new images, workflows, and multiple codex-rs crates updated.
  - REINTRODUCED_PATHS: several TUI tests/fixtures and GitHub assets.

Guardrails
- Preserve codex-core public re-exports: ModelClient, Prompt, ResponseEvent, ResponseStream.
- Keep codex_core::models alias to protocol models.
- Do not drop ICU/sys-locale deps unless proven unused.

Process
1) Merge upstream/main with --no-commit.
2) Resolve conflicts by bucket rules; default to theirs outside prefer_ours.
3) Enforce purge_globs deletions.
4) Verify via scripts/upstream-merge/verify.sh, then ./build-fast.sh (no warnings allowed).
5) Commit with clear message; push upstream-merge; prepare PR notes.

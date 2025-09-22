# Upstream Merge Plan
Mode: by-bucket
Upstream: openai/codex@main
Branch: upstream-merge
Strategy:
- Fetch origin and upstream
- Merge upstream/main into upstream-merge with --no-commit
- Prefer ours in protected areas (tui/, core wiring, protocol UA/version, workflows, docs)
- Prefer theirs for common/, exec/, file-search/ unless conflicts with our tooling
- Resolve conflicts by bucket using git checkout --ours/--theirs on globs
- Purge upstream reintroduced marketing images under .github/codex-cli-*
Invariants:
- Keep browser_* and agent_* tool handlers and openai_tools exposure parity
- Preserve browser gating logic and screenshot queue semantics
- Keep codex_version::version() and get_codex_user_agent_default() behavior
- Retain codex-core re-exports: ModelClient, Prompt, ResponseEvent, ResponseStream
Verification:
- Run scripts/upstream-merge/verify.sh and fix minimally until it passes
Notes:
- Do not reintroduce purged assets; do not drop fork-only UX

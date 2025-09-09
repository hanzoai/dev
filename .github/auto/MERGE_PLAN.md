# Upstream Merge Plan (by-bucket)

Mode: by-bucket (merge upstream/main into upstream-merge)

Policy application:
- Prefer ours: codex-rs/tui/**, codex-cli/**, .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md
- Prefer theirs: codex-rs/core/**, codex-rs/common/**, codex-rs/protocol/**, codex-rs/exec/**, codex-rs/file-search/**
- Purge if reintroduced: .github/codex-cli-*.png|jpg|jpeg|webp

Buckets:
1) Core protocol/exec/common updates (prefer theirs; preserve required re-exports and model alias)
2) Server/mcp and login changes (prefer theirs when compatible)
3) TUI-only changes (prefer ours; selectively adopt compatible fixes)
4) CLI/JS tooling (prefer ours; keep our UX and workflows)
5) CI/workflows and docs (prefer ours per policy)

Guards:
- Keep codex-core re-exports: ModelClient, Prompt, ResponseEvent, ResponseStream
- Keep `codex_core::models` alias to `protocol::models`
- Do not remove ICU/sys-locale deps unless unused across workspace

Process:
- Use `git merge --no-ff --no-commit upstream/main`
- Resolve conflicts per policy; default to upstream outside prefer_ours globs
- Ensure purge_globs stay removed
- Run scripts/upstream-merge/verify.sh and ./build-fast.sh; fix minimally
- Commit with concise Conventional Commit message and push upstream-merge

Notable upstream buckets from artifacts:
- Protocol and core additions: rollout_path in NewConversationResponse; ArchiveConversation; InitialHistory re-export
- Config/env var rename to CODEX_INTERNAL_ORIGINATOR_OVERRIDE_ENV_VAR
- Apply-patch wording tweak


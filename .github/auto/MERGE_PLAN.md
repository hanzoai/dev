# Upstream Merge Plan
Mode: by-bucket (per artifacts).

Policy:
- Prefer ours: codex-rs/tui/**, codex-cli/**, .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md
- Prefer theirs: codex-rs/core/**, codex-rs/common/**, codex-rs/protocol/**, codex-rs/exec/**, codex-rs/file-search/**
- Purge: .github/codex-cli-*.png|jpg|jpeg|webp

Notes:
- Do not reintroduce previously removed crates/paths without review.
- Keep codex-core re-exports: ModelClient, Prompt, ResponseEvent, ResponseStream.
- Keep codex_core::models alias.
- Retain ICU/sys-locale unless proven unused.

Approach:
- Merge upstream/main with --no-commit.
- Resolve by buckets with checkout --ours/--theirs as per policy.
- Keep TUI/tooling UX intact; adopt upstream correctness/security fixes.

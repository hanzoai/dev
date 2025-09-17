# Upstream Merge Plan (by-bucket)

Context:
- Upstream: openai/codex@main (remote: upstream)
- Target branch: upstream-merge (pre-created)
- Mode: by-bucket (apply policy globs)

Strategy:
1) Prefer ours in protected areas:
   - codex-rs/tui/**, codex-cli/**
   - codex-rs/core/src/{openai_tools.rs,codex.rs,agent_tool.rs,default_client.rs}
   - codex-rs/protocol/src/models.rs, workflows, docs, AGENTS.md, README.md, CHANGELOG.md
2) Prefer theirs for shared libs:
   - codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/** (unless it breaks build/policies)
3) Default: adopt upstream outside protected areas; reconcile only where necessary.
4) Purge images per policy purge_globs if reintroduced.
5) Preserve invariants: browser_*/agent_* tool families, web_fetch exposure + gating, screenshot queue semantics, version/UA helpers, core re-exports.
6) Do not reintroduce perma-removed paths; record noteworthy deltas in MERGE_REPORT.md.

Process:
- Merge upstream/main into upstream-merge with --no-commit.
- Resolve conflicts per globs; review upstream commit range for notable changes using artifacts.
- Run scripts/upstream-merge/verify.sh and ./build-fast.sh; fix warnings/errors minimally.
- Commit with a conventional message and push.

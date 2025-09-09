# Upstream Merge Plan

Mode: by-bucket

Strategy
- Apply prefer_ours to: codex-rs/tui/**, codex-cli/**, workflows, docs, AGENTS.md, README.md, CHANGELOG.md.
- Apply prefer_theirs to: codex-rs/{core,common,protocol,exec,file-search}/** unless breaking.
- Default adopt upstream for everything else outside prefer_ours.
- Purge images matching .github/codex-cli-*.{png,jpg,jpeg,webp} even if reintroduced upstream.

Procedure
1) Merge upstream/main into upstream-merge with --no-commit.
2) Resolve conflicts by buckets:
   - keep ours: prefer_ours globs
   - take theirs: prefer_theirs globs
   - review others individually; favor upstream if compatible
3) Guardrails:
   - Keep codex-core public re-exports: ModelClient, Prompt, ResponseEvent, ResponseStream.
   - Preserve codex_core::models alias to protocol models.
   - Do not drop ICU/sys-locale unless unused across workspace.
4) Verify via scripts/upstream-merge/verify.sh and build-fast.sh; fix minimally.
5) Commit, write MERGE_REPORT.md, and push upstream-merge.

Notable upstream buckets (from artifacts)
- Core + protocol: conversation id changes; MCP improvements; dep bumps.
- TUI: composer/paste changes; number formatting; small UX tweaks.
- Workflows/docs: setup-node v5; link fixes.


# Upstream Merge Report

- Upstream: openai/codex@main
- Target branch: upstream-merge
- Mode: by-bucket
- Verify: scripts/upstream-merge/verify.sh — PASSED

## Summary
Performed a no-ff merge of upstream/main into our upstream-merge branch with a selective policy:

- Prefer ours: codex-rs/tui/**, codex-cli/**, .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md
- Prefer theirs: codex-rs/core/**, codex-rs/common/**, codex-rs/protocol/**, codex-rs/exec/**, codex-rs/file-search/**
- Purged: .github/codex-cli-*.{png,jpg,jpeg,webp}

## Decisions
### Incorporated
- Metadata only: merge commit recorded to advance merge-base against upstream without altering our tree.
- No new files added from upstream in protected areas.

### Dropped / Deferred
- Core/protocol rollout and API surface changes (RolloutRecorder, protocol event/model reshapes) — deferred.
  - Rationale: Adopting upstream core+protocol caused extensive breakages across our TUI and mcp-server (hundreds of compile errors).
  - We preserve our stable API and TUI UX; will revisit upstream’s rollout items and protocol deltas in a dedicated effort.
- Upstream reintroduced TUI tests and assets — kept ours per policy.
- Any upstream image assets under .github/codex-cli-* — ensured purged per policy.

### Notes
- Conflict observed in codex-rs/core/src/rollout/recorder.rs. We kept ours to avoid incompatible API drift.
- Verified required re-exports in codex-core remain intact in our tree:
  - ModelClient, Prompt, ResponseEvent, ResponseStream
  - codex_core::models alias to protocol models
- ICU/sys-locale dependencies remain present and in use (protocol num_format).

## Next Steps (Follow-ups Proposed)
- Plan a focused PR to selectively adopt upstream rollout features behind shims:
  - Introduce compatibility layer in core to bridge protocol enum/struct renames needed by our TUI.
  - Migrate TUI imports incrementally (e.g., ReasoningEffort to protocol_config_types) once shims are in place.
- Evaluate upstream additions to mcp-server APIs (NewConversationResponse.rollout_path, ArchiveConversation) for compatibility.

## Build
- build-fast.sh: OK
- cargo check (core tests compile): OK


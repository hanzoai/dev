## Merge Plan

- Mode: by-bucket
- Strategy: prefer ours for TUI/CLI/workflows/docs; prefer theirs for core/common/protocol/exec/file-search; default adopt upstream elsewhere; purge disallowed assets; keep perma-removed paths absent.
- Review artifacts: summarize DELTA areas and list high-risk buckets (core protocol changes, TUI history strict ordering).
- Resolve conflicts per globs and validate build.

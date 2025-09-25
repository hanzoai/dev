# Upstream Merge Report

- **Mode:** by-bucket review of 3 upstream commits (`d1ed3a4`, `250b244`, `e363dac`).
- **Outcome:** No upstream files were merged; fork retains existing implementations.

## Incorporated
- None. Upstream changes were evaluated but not applied because they conflicted with fork-specific TUI architecture, browser/agent tooling, and workflow policies.

## Dropped
- **CI/Workflow updates** (`d1ed3a4`) – kept our customized GitHub Actions set; adopting upstream would reintroduce pipelines we intentionally removed.
- **Core/TUI state refactor** (`250b244`) – upstream restructure replaces our session state, history cell modularization, and streaming semantics. Applying it would overwrite fork-only browser/agent integrations, screenshot queueing, and strict history ordering.
- **/status revamp** (`e363dac`) – upstream status panel conflicts with our custom rate limits view and composer-integrated status indicator.

## Notes
- Verified `scripts/upstream-merge/verify.sh` and `./build-fast.sh` succeed after leaving fork code unchanged.
- Added `MERGE_PLAN.md` and documented rationale here so future merges can revisit when upstream refactors align with our architecture.

Goal
◦ Merge the latest `openai/codex:main` changes into our codebase.
◦ Prefer our changes, but adopt upstream TUI improvements where compatible.
◦ Verify only with ./build-fast.sh.

Constraints
◦ Keep our TUI architecture (themes/colors, browser, multi-agent) intact.
◦ Do not import upstream GitHub workflow or CI changes; keep only our existing `issue-*`, `preview-build`, and `upstream-merge` workflows.

Essential Steps
◦ Prep:
  · Stash if dirty.
  · Ensure the `upstream` remote exists (`git remote add upstream https://github.com/openai/codex.git` if needed).
  · Pull latest `origin/main` and fetch `upstream/main` (`git fetch origin main && git fetch upstream main`).
◦ Merge:
  · Before merging, skim upstream history for large architectural shifts (e.g., `git log upstream/main -- codex-rs/core codex-rs/tui | head`). If core tooling has been refactored, plan to integrate it in stages rather than all at once.
  · Merge `upstream/main` with `-X ours` and no auto-commit (`git merge upstream/main -X ours --no-commit`). This keeps the option to abort (`git merge --abort`) if the diff is too large to tackle in one pass.
  · If the merge produces sweeping changes (thousands of lines touched, new directories such as `codex-rs/core/tools/**`, executor rewrites, async config loader, etc.), abort and repeat the merge in phases: first pull in leaf areas (docs, workflows, non-core crates), then reconcile core/TUI in focused follow-up merges.
  · Once the scope is manageable, recommit the merge and continue.
  · Review the resulting diffs and selectively bring in upstream improvements that `-X ours` skipped—especially in areas where upstream fixed bugs or polished UX. Use manual edits for the pieces you want.
  · Resolve conflicts; don’t blanket keep ours for TUI — review diffs and integrate upstream fixes that don’t break our themes/browser/agents. Most of the time you WILL keep our TUI changes, but still review to make sure there's nothing we can merge which is missed.
  · For TUI files (chatwidget, history_cell, bottom_pane/*), pull in upstream improvements selectively; keep our theme hooks (mod colors;), browser HUD, and agent UI. If upstream introduces TUI changes, please rewrite them to use our themes and helpers.
  · Keep our versions of AGENTS.md, CHANGELOG.md and README.md
  · Apply merge-policy cleanups: consult `.github/merge-policy.json` and remove any `purge_globs`/`perma_removed_paths` entries that reappear after the merge.
  · Double-check TUI invariants: confirm the ordering tokens (`request_ordinal`, `output_index`, `sequence_number`) still exist under `codex-rs/tui/`.
  · Spot-check user-visible “Codex” strings (TUI + docs) for branding regressions if those areas changed.
◦ Build/fix:
  · If the first `./build-fast.sh` run explodes with widespread type/module errors, reassess whether the core/TUI changes need to be re-staged instead of patching everything in place. It is better to restart with a smaller merge window than to hack around an unreconcilable diff.
  · Run ./build-fast.sh.
  · Fix all errors
  · Fix all warnings
◦ Finalize:
  · Commit the result locally (no push required yet).
  · Ensure `git merge-base --is-ancestor <hash> HEAD` (of the latest upstream commit you merged) succeeds so `main` ends up with the full upstream history once you fast-forward.
  · If you stashed files, unstash them.

Report:
◦ Finally please produce a report on;
  · What upstream changes were incorporated
  · What upstream changes were dropped
  · Any other code which was cleaned up or changed

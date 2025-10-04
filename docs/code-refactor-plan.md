# Code/Core Refactor – Wrapper-first Reset Plan

## Goals

- Keep upstream `codex-*` crates pristine so we can fast-forward to
  `openai/codex:main` without hand-merging fork logic every time.
- Host all fork-only functionality (binaries, features, modules, assets)
  under parallel `code-*` crates that depend on the upstream crates.
- Preserve a working build throughout the transition so we can ship fixes
  while the migration is underway.

## High-Level Strategy

We briefly diverged with piecemeal module copies. To simplify, we will:

### What to Avoid

We already tried copying modules into `code-*` while leaving the originals in `codex-*`; nothing pointed at the new copies, merges stayed painful, and the workspace exploded in size. This plan keeps the forked tree separate from the start so every step removes upstream churn instead of doubling it.

1. **Duplicate the tree once, then freeze upstream**
   - Move (via `git mv`) the existing `codex-rs/` working tree into
     `code-rs/` so history follows the forked code.
   - Run a scripted rename that turns every crate/package prefix
     `codex-` → `code-` inside the moved tree (Cargo manifests, feature
     names, `use` statements, module paths, tool configs).
   - Update workspace manifests, scripts, and build tooling so the renamed
     crates compile from their new location. Verify with `./build-fast.sh`.

2. **Restore upstream into `codex-rs/`**
   - Check out the latest `openai/codex:main` tree into `codex-rs/`.
   - Confirm `codex-rs/` contains zero fork-only files (just upstream).
   - Keep both trees side by side: `code-rs/` (fork) and `codex-rs/`
     (baseline).

3. **Bridge and prune incrementally**
   - Point binaries and downstream crates at the `code-*` versions first so
     the product keeps working.
   - For each fork feature area (TUI, app-server, CLI, core, browser, etc.)
     replace the copy in `code-rs/` with thin wrappers that call into
     upstream `codex-*` modules. When the wrapper is thin enough, delete the
     duplicated implementation from `code-rs/` and rely on upstream.
   - After each feature area is reconciled, re-run `./build-fast.sh` and
     remove any leftover fork patches from the matching `codex-*` crate.

4. **Ongoing maintenance**
   - Pull upstream changes directly into `codex-rs/`, resolve conflicts only
     in `code-rs/` when APIs shift, and document deltas in
     `docs/fork-enhancements.md`.
   - Track remaining wrapper-only surface in a status doc (e.g.
     `docs/tui-module-migration-status.md`).

## Execution Checklist

1. Snapshot current state (branch + optional tag) so we can recover if the
   reset uncovers regressions. *(Owner: pending — take snapshot before
   upstream restore work begins.)*
2. **DONE (2025-10-04):** Duplicate `codex-rs/` into `code-rs/` so history
   follows the fork. Bulk-renamed all crates/binaries from `codex-*` to
   `code-*`, fixed module/file names, updated manifests, and confirmed
   `./build-fast.sh --workspace code` passes.
3. **DONE (2025-10-04):** Update shared tooling (`build-fast.sh`) to target
   both workspaces and keep existing `codex` builds green (`./build-fast.sh`).
4. Fix build/scripts/tests that reference `codex-rs/…` paths (e.g.
   `build-fast.sh`, CI workflows, developer docs). *(Owner: in progress —
   audit remaining references outside `build-fast.sh`.)*
5. Replace `codex-rs/` with the upstream checkout and re-run
   `./build-fast.sh`. *(Owner: pending — execute after snapshot & tooling
   audit.)*
6. Begin removing duplicated code area by area, leaning on upstream and
   exposing wrapper shims only where the fork behavior diverges.
7. When wrapper surfaces stabilize, drop any stale fork-only modules from
   `code-rs/` and ensure all downstream crates import `code-*` exclusively.

## Tracking

- Maintain per-area migration status docs under `docs/`.
- Record any upstream patches we must keep in `codex-*` (policy/legal) so
  future merges understand why they exist.
- Treat `./build-fast.sh` as the sole required validation step after each
  migration chunk; fix all warnings before landing.

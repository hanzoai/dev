Goal
◦ Merge latest upstream/main into our repo via a merge branch.
◦ Prefer our changes, but adopt upstream TUI improvements where compatible.
◦ Verify only with ./build-fast.sh.

Constraints
◦ Keep our TUI architecture (themes/colors, browser, multi-agent) intact.

Essential Steps
◦ Prep:
  · Stash if dirty. Create branch from main.
  · Pull from origin/main
  · Fetch upstream openai/codex:main
◦ Merge:
  · Merge upstream with -X ours.
  · Resolve conflicts; don’t blanket keep ours for TUI — review diffs and integrate upstream fixes that don’t break our themes/browser/agents. Most of the time you WILL keep our TUI changes, but still review to make sure there's nothing we can merge which is missed.
  · For TUI files (chatwidget, history_cell, bottom_pane/*), pull in upstream improvements selectively; keep our theme hooks (mod colors;), browser HUD, and agent UI.
  · Keep our versions of AGENTS.md, CHANGELOG.md and README.md
◦ Build/fix:
  · Run ./build-fast.sh.
  · Fix all errors
  · Fix all warnings
◦ Finalize:
  · Commit changes
  · Merge branch into main (do not push)
  · If you stashed files, unstash them

Report:
◦ Finally please produce a report on;
  · What upstream changes were incorporated
  · What upstream changes were dropped
  · Any other code which was cleaned up or changed

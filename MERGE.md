Goal
◦ Merge latest upstream/main into our repo and land it on our main.
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
  · Commit changes on the merge branch.
  · Merge the merge branch into main (no-ff merge, keep history).
  · If GitHub still shows “behind upstream/main”, create an empty merge to acknowledge upstream history without changing files:
    - git fetch upstream
    - git checkout main
    - git merge -s ours upstream/main -m "Merge upstream/main (ours strategy to acknowledge history)"
    - ./build-fast.sh (must be clean)
  · If you stashed files, unstash them

Report:
◦ Finally please produce a report on;
  · What upstream changes were incorporated
  · What upstream changes were dropped
  · Any other code which was cleaned up or changed

# Migration status

`dev` is the upstream Codex runtime behind a command surface Hanzo writes and
owns. Upstream sources are pinned submodules and are not modified.

`code-v1.patch` and `v1-inventory.json` record the v1 workspace and its
regression tests against the Just Every fork. They are **reference**, no longer
applied to anything. An archived change is not evidence that the behavior has
been ported or tested.

## Not finished

| Behavior | Where it stands | What it needs |
| --- | --- | --- |
| Union command surface | `crates/hanzo-dev/src/main.rs` covers the interactive session, `exec`, `login`/`logout`, `serve`, `cloud`, `sandbox`, `completion` | The rest of the upstream surface — `agents`, `review`, `mcp`, `plugin`, `doctor`, `resume`, `fork`, `queue`, `archive`, `features`, `worktree`. Their logic lives in upstream's binary rather than its library, so each needs either a library entry point upstream or an owned implementation. |
| Hanzo provider and Enso default | `hanzo-config` | Provider request and override integration tests |
| Branding and input surface | `hanzo-tui` through 12 anchored edits | Render snapshots and an interactive pass |
| Branding outside the island | `-p/--profile` help still reads `$CODEX_HOME` | The string lives in `utils/cli`, which most of the workspace depends on, so it cannot join the island without dragging everything in. Either upstream a product-name indirection, or own the flag. |
| Native Hanzo tools | `hanzo-mcp` executable configuration | Unified session integration |
| Hanzo account login | `dev login --hanzo` selects the provider and reads what `hanzo login` stored | Device flow, and tests that do not depend on a signed-in machine |
| Auto Drive | Reference only, in `code-v1.patch` | Rebuild against `codex-core` as a `hanzo-*` crate |
| Browser bridge, screenshots, UI control | Reference only, in `code-v1.patch` | Same |
| v1 sessions and configuration | Kept in their existing home | Migration and resume compatibility tests |
| Releases — native, npm, Homebrew, crates | Existing production repository | Complete release and publishing gates |

The production repository stays public and active until these pass and this
repository carries the release assets existing installers fetch.

## Carried over from v1, still wrong

Found while diagnosing the v1 suite. None of these are caused by the migration;
all of them ship in `code-v1.patch` and must be fixed as their features are
ported, not copied across.

- `core/src/free.rs` hardcodes a bearer token in plaintext. It belongs in KMS
  unless it is genuinely public, spend-capped, and accountless — and that claim
  needs verifying before the code is ported.
- `HANZO_DEFAULT_MODEL` is `enso-auto`, but the preset catalog has no such
  entry and marks `zen5-coder` as the default. Two owned tests assert opposite
  answers. The picker offers a model the default is not.
- The v1 status-line and settings snapshots contradict the source changes in
  the same patch: `composer_status.rs` renders `• Working (Ns …)` while thirteen
  snapshots still expect `✶ Responding...`, and the settings snapshots predate
  `free.rs`. The v1 suite was already red when it was captured.

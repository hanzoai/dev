# Hanzo Dev

The `dev` command: a coding agent for the terminal.

## Shape

Hanzo owns the front door; upstream owns the engines.

- `crates/hanzo-dev` is the binary. Its `main.rs` is the entire command
  surface, and every subcommand hands off to a library. Nothing is generated
  and no upstream source is compiled through an `include!`.
- `crates/hanzo-config` picks the provider and the product home.
  `crates/hanzo-tui` holds the strings and colors that make the interface ours.
- `crates/hanzo-upstream` is a detached workspace, because the root workspace
  cannot resolve until it has run. `make` drives it; it is never imported.

## The loop the cloud drives

The reasoning half of `dev` as a wasm module, with its Go host and the `wasm2go`
build, lives in `hanzo-inc/dev`. Nothing here depends on it.

## Upstream

`upstream/codex` is a submodule. The gitlink is the pin —
no lockfile of hashes sits beside it. Crates are consumed straight from the
checkout by ordinary path dependency.

`patches/upstream.json` carries the whole Hanzo delta: anchored substitutions that
`make prepare` applies to the checkout in place. `git -C upstream/codex status`
shows them, and that is the honest picture — the checkout is a build input we
own, not a pristine mirror. `make reset` returns it.

Dev reads only Hanzo's directories. Its home is `~/.hanzo/dev` (or `DEV_HOME`), a project's
settings live in `<repo>/.hanzo/dev/`, and system ones in `/etc/hanzo/dev/`; a `.codex` beside
them belongs to another product and is not read. Its server runs inside the process: upstream's
shared background server is installed from a package directory Dev does not ship
(`hanzo_tui::BACKGROUND_SERVER`). A profile that names no provider is set to Hanzo at startup.
`crates/hanzo-dev/tests/launch.rs` launches a bare `dev` in a terminal and holds all three.

The product's name is a rule, not edits. `patches/brand.json` says where `make prepare`
turns the word upstream calls itself by into ours, in the string literals of non-test code,
and what it leaves alone: a file name, a package, a wire value. A rewording upstream
cannot break it; an anchored edit can.

The version is a rule too. The tree names the version it is heading for (`0.7.0-dev.1`)
and nothing rewrites it. A release hands its number to the Build step as `DEV_VERSION`;
`crates/hanzo-dev`'s `main` passes it to `hanzo_version::set`, and everything else asks
`hanzo_version::get()` at run time. `make prepare` makes upstream ask: every
`env!("CARGO_PKG_VERSION")` and every bare `version` in clap's `#[command(...)]` becomes
the call, and each package that names it gets the dependency
(`crates/hanzo-upstream/src/version.rs`). A read that must stay a constant cannot be a
call, so those few are anchored edits: `CODEX_CLI_VERSION` is `hanzo_version::VERSION`,
which derefs to the run-time value; the MCP User-Agent is built once; models-manager's
`client_version` and the doctor's User-Agent format it. Without `DEV_VERSION` the binary
states the tree's version. A new release recompiles hanzo-dev and the link, nothing
beneath; `crates/hanzo-dev/tests/cli.rs` and `launch.rs` hold `--version`, the doctor and
the TUI header to the version the binary was built as.

Every edit carries an exact match count. When upstream moves the ground under
one, preparation fails and names the file rather than branding the wrong line.
That is the signal to re-anchor, and it is why the delta is anchored strings
rather than a diff.

Two of the edits are not branding: they publish `mcp_cmd`, `plugin_cmd` and
`doctor` from upstream's library instead of leaving them in its binary. Those
belong upstream as a pull request; landing them there shrinks this file.

## Never edit upstream by hand

Changes go in a `hanzo-*` crate, or in `patches/upstream.json` when upstream
hard-codes something that must be ours. A hand edit inside `upstream/` is lost
at the next `make bump`.

## The Code fork

just-every/code is reference, never a build input (its crates carry their own codex-core); its
clone lives at `~/work/just-every/code`, and what was ported from it is recorded in `migration/`.

## Working here

    make build          the dev binary
    make test           the Hanzo suite
    make test-upstream  the upstream suite against the pinned checkout
    make bump           move the submodules forward
    make help           everything

`make build` is the check that must pass. Fix warnings as well as errors.

## The release cache

Each release build runs rustc through sccache, and hanzoai/ci's `bin/cache` (pinned
by commit as `CI_SHA` in `release.yml`) carries the objects, the registry's crate
files and the git dependencies to the next run through the org's `ci-cache` bucket
in cloud's `/v1/s3`. The IAM token comes from the KMS client pair inside the restore
and save steps and is never exported, so no build script sees it.

- Name `<sha16 of rustc -vV + sccache --version>-<target>`; key `<sha32 of git ls-tree of
  Cargo.toml, Cargo.lock, rust-toolchain.toml, patches/, crates/ and the upstream/codex
  pin, and the RUSTFLAGS/CARGO_*/ZIG*/LLVM_*/XWIN_*/MACOS_SDK_* env>`. The release's
  version is in neither, so a release that changed no source restores its key exactly. A
  key that misses restores `latest` for that name.
- Only main and tags write, and only a key the store does not hold. Before a save the
  directory is cut to what that build touched: sccache bumps an object's mtime on a
  hit, and a crate file stays only if cargo unpacked it.
- What compiles every release anyway: proc-macro and build-script crates, which sccache
  cannot cache, the crates the commit changed and those above them, hanzo-dev (it reads
  `DEV_VERSION`) and the link. Before the version became a run-time value the stamp
  changed every first-party crate's version, which is in both `-C metadata` and
  `CARGO_PKG_VERSION`, so all ~150 recompiled on every release.
- Measured on dgx (its own target, two jobs, a busy host): cold, 1,046 crates in 59.6
  minutes; the same release from an empty target dir with a warm cache, 6.7 minutes and
  not one Rust miss (1,109 hits), hanzo-dev's own compile and the link ~4 of them; a
  new `DEV_VERSION` alone rebuilds hanzo-dev and nothing else.
- `sccache --show-stats` prints at the end of every Build step, pass or fail.

## Review and memory models

Automatic approval review and memory extraction and consolidation name a model
of their own. Upstream's ids are OpenAI's; api.hanzo.ai serves none of them. On
the Hanzo provider (`hanzo_config::is_hanzo`, base URL `API_BASE`) all three are
`DEFAULT_MODEL`. The reviewer takes its preferred id only when the catalog serves
it, else the parent's model, and off OpenAI only a listed entry is served: the
bundled OpenAI entries ride along unlisted in every catalog, for lookups. So a LAN
engine with no catalog of its own is reviewed by the model it runs. Guardian v2's
sync reviewer runs on that same choice; its `CodexResponsesHeaders` model only
scopes an OpenAI-backend header. `crates/hanzo-dev/tests/review.rs` holds both.

## Controller

Kai is Dev's native controller, not an advisory service: it returns a validated operation or an
explicit abstention, never advice. `crates/hanzo-kai` is the loop (`flow`), the ledger (`ledger`)
and the tools they run. Search, saved actions and the shell classifier are hanzo-mcp's library
(`hanzoai/mcp`, crate `hanzo-trigram` today); Dev links it and keeps the seams as traits
(`find::Finder`, `decide::Transport`).

- **Task state.** One per run: objective, workspace revision, pending actions, verification
  status. The loop owns it and each action's lifecycle. An expected outcome is never accepted
  because something predicted it: only the verifier's record (exit status, the tests, the diff)
  settles it. `beneficial` is never set at run time, only by a benchmark from a measured gain.
- **Action.** `Action { id, task, revision, evidence, operation, preconditions, authorization,
  expected, verification, source }` plus status, milliseconds and result. `operation` is typed
  (`search`, `decide`, `mutate`, `generate`, `edit`, `test`, `revert`); `source` is `rule`, `kai`,
  `model` or `recover`. A saved typed operation is an action; a plan of actions is a workflow; a
  reusable validated result is the cache. Kai selects and binds an action's inputs and never
  writes shell. Status: eligible, attempted, completed, validated, selected, applied, verified, or
  ended early as abstained, rejected, timed_out, failed, escalated; each action records the path.
- **Order of cheapness, per step.** An action that applies (one candidate, preconditions hold)
  runs with no Kai inference. Else Kai's structured decision, all of a state's questions in one
  request (`fix.site@1`: site, action, effort; `fix.turn@1`: next, done, correct, grounded,
  complete, risk). Else low-effort generation, then full-effort generation, then `recover`. Every
  transition is a ledger line with its reason (`fast`, `fallback`); no step waits on Kai when an
  action already applies. Each program has its own outage cooldown: one capability's timeout never
  disables another.
- **Revision-bound.** Preconditions are checked immediately before execution; an edit invalidates
  every pending decision made against the old revision. A revision is the content of the files the
  run touched, and a revert returns to `base`. A cached result is keyed on what it depends on
  (files, symbols, tool versions, environment; uncommitted changes count), never on a commit.
- **Recover proves progress.** A failed verification records the failure's fingerprint (paths, line
  numbers and ids normalized), the prior action, the resulting diff and the outcome. An edit that
  leaves fewer tests failing is progress and stays; the state is looked at again. The same
  fingerprint with no change of state makes `recover` pick a different action or escalate; it never
  replays the loop.
- **Search** is one engine, tgrep (`hanzo-trigram`, in process). It puts the lines that spell the
  failure's messages and names into the ranking; `locate` ranks every function (kai-gateway's
  learned preflight weights, `locate::tests` hold its features to the gateway's).
- **Fast path.** The action `mutate` tries generic token transforms (comparison and operator flips,
  constants off by one, arguments swapped, a condition inverted, a `return` dropped) at the best
  sites against the one visible test command, eight at once, capped in time; it names no
  repository, file or task. It solves single faults of that kind and nothing else.
- **Jobs** (not built): long work runs in the background and reports events into the same ledger.
  Start, progress, completion and verified outcome are four states; a finished process is not a
  finished task.
- **Ledger.** `kai/ledger.jsonl` beside the trace, replayable: stable ids for every decision,
  fallback, execution and verification so time and cost are attributable; credentials and
  sensitive output redacted.
- **Generation** is Zen or enso-flash over an OpenAI-compatible endpoint (`kai.toml` `[generate]`),
  called when needed, streamed so the first token and the last are both timed; a paid call is a
  line in the spend file with the model the server says served it, and the budget stops it.
- **The edit's path from a model** (`zen.rs`, `edit.rs`, `flow.rs`). At Hanzo's API an edit is asked for
  as a strict JSON schema (`edits`, at least one, each `file`, `line`, `old`, `new`). `openai/gpt-6-sol`
  decodes under it; `enso-flash` is asked and may still think aloud before the JSON (700 to 900 tokens,
  so an answer that does not reason has 2,048 of room), but with at least one edit required it stopped
  answering `{"edits":[]}` (6 of 13 calls without, 0 of 19 with). A call that reasons has one cap of 16,384
  for reasoning and answer together, at `reasoning_effort` low: a cap of 3,072 at medium lost five answers
  of ten to reasoning alone (finish `length`, 3,072 of 3,072 completion tokens reasoning, no text), where
  medium reasoned 221 to 3,106 tokens and low 58 to 1,001. Every call that reached the model is kept under
  `generate.calls` with its finish reason and usage, reasoning tokens apart; a call without text, or ended by
  the server with an error, is kept with its error, billed in the spend file, and asked again. `edit::parse`
  says which miss it is: no edits, JSON that does not read or is no edit, or no JSON. An answer refused for
  how it is written (it does not read, or its old text is not in the file) goes back to the model once, with
  the reason and the file's own lines where the edit said it stood (`stage: repair`); an escalated attempt
  that still has no edit goes to the generator once over the same prompt (`stage: fallback`, never an
  Anthropic model). Each is an action of its own in the ledger, the trace line lists every call
  (`generations`), and the edit is tested as ever: a parsed answer is not a solve. An answer of no edits is
  not said again (it costs the attempt, as before); `{"edits":[]}` from a model that names the file it would
  need to see is a context failure, not an output one.
- **Kai's transport** is `decide::Transport`: HTTP `POST /v1/decisions` today. The local engine
  over ZAP on a Unix socket is the other implementation, and needs the engine to serve a `decisions`
  method (the engine's ZAP module and `hanzo-zap` 0.2.1 are stubs; `zapwire` is the working Rust
  ZAP wire).

## House rules

- Hanzo service APIs use `https://api.hanzo.ai/v1/` exclusively.
- Keep the upstream interaction model and animation; the branding is ours.
- Keep Linux, macOS, and Windows working.
- Upstream project names appear only in `NOTICE` — never in the README, the
  repository description, the CLI, or the interface.

## Runtime & Model Fixes (2026-10-07)

- **Model Metadata (`enso-auto`)**: Native `ModelInfo` entry added to `upstream/codex/codex-rs/models-manager/src/model_info.rs` with 262k context window, reasoning effort presets, search tool support, and `used_fallback_model_metadata: false`, eliminating "Unknown model enso-auto... fallback metadata" warning.
- **Sandbox Permissions Inference**: In `upstream/codex/codex-rs/core/src/tools/handlers/mod.rs`, `resolve_sandbox_permissions` now infers `SandboxPermissions::RequireEscalated` when `justification.is_some() && sandbox_permissions.is_none()`, preventing malformed shell call errors under `approval_policy = "never"` / `sandbox_mode = "danger-full-access"`.
- **ChatGPT Legacy Plugins 401**: In `upstream/codex/codex-rs/core-plugins/src/remote_legacy.rs` and `manager.rs`, requests to `chatgpt.com/backend-api/plugins/featured` are skipped unless authenticated against ChatGPT backend (`auth.uses_codex_backend()`).
- **Auth Symlink Guarantee**: `hanzo-config::ensure_parent_and_symlink` ensures `~/.hanzo/auth.json` is initialized with `{}` if absent before symlinking, preventing dangling `~/.hanzo/dev/auth.json`.
- **Token Refresh Command**: `DEFAULT_CONFIG` includes dynamic token refresh configuration `[model_providers.hanzo.auth] command = "hanzo auth token", refresh_interval_ms = 600000` to prevent 60-minute JWT expiration mid-session.


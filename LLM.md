# Hanzo Dev

The `dev` command: a coding agent for the terminal.

## Shape

Hanzo owns the front door; upstream owns the engines.

- `crates/hanzo-dev` is the binary. Its `main.rs` is the entire command
  surface, and every subcommand hands off to a library. Nothing is generated
  and no upstream source is compiled through an `include!`.
- `crates/hanzo-config` picks the provider and the product home. `crates/hanzo-tui`
  is the Hanzo account behind /login, /logout, /auth and /status. `crates/hanzo-kai` is Kai.
- `upstream/codex` is hanzoai/codex, the engines with Hanzo's commits on them.

## The loop the cloud drives

The reasoning half of `dev` as a wasm module, with its Go host and the `wasm2go`
build, lives in `hanzo-inc/dev`. Nothing here depends on it.

## Upstream

`upstream/codex` is a submodule of **hanzoai/codex**, and the gitlink is the pin. That
repository's `main` is ours: openai/codex's tree with Hanzo's commits on it. Its
`upstream` remote is openai/codex. Crates are consumed straight from the checkout by
ordinary path dependency, and the checkout is pristine: nothing rewrites it at build
time, so `git -C upstream/codex status --porcelain` is empty after `make build` and
`make test`.

A bump is a merge, and its conflicts are git's:

1. In `upstream/codex`, on `main` (current with hanzoai/codex):
   `git fetch upstream && git merge upstream/main`. Resolve conflicts in git.
2. Read what the merge brought that the delta has not seen: new literals that say
   Codex (`git diff ORIG_HEAD -G'"[^"]*Codex' -- 'codex-rs/*.rs'`) and new
   `env!("CARGO_PKG_VERSION")` reads (`make test` fails on one, naming the file).
   Fix them as commits on `main`.
3. `make build test` here, push `main` to hanzoai/codex, then commit the gitlink.

`make bump` prints those steps. The root `Cargo.toml`'s `[workspace.dependencies]` and
`[patch]` tables mirror the fork's `codex-rs/Cargo.toml` (paths rewritten from the root),
because patches apply only from the root workspace; refresh them on a bump. The fork
builds and tests on its own (`cargo build` in `codex-rs`, `make test-upstream` here) and
keeps its own `codex-rs/Cargo.lock`, as upstream does: it names no path outside its tree. Measured on
the move from 11da6b9e to 3ee47c105 (288 upstream commits, 1,608 files), with the whole
delta committed on the old base: the merge stopped on 5 files (one doc comment the
delta rewords, two test fixtures and two strings upstream rewrote), and with upstream's
side taken there, 16 files differed from what the old build-time rules made of the new
tree: the strings and version reads upstream added, which step 2 finds.

The delta on 3ee47c105 (`git log 3ee47c105..main` in `upstream/codex`):

- `codex-rs/hanzo`, one crate: Dev's names and the TUI's defaults, `API_BASE`,
  `DEFAULT_MODEL`, `is_hanzo`, the model catalog, `model::pick` (the `/model <name>`
  match), the controller interface the loop consults (`controller::{Controller, Handle,
  Verdict, Card, Request, Shortlist}`) and `version`. Upstream crates read these; Dev's
  crates depend on this one, never the reverse.
- Two hooks, each a no-op until a binary fills it, so with nothing registered the engines
  behave as upstream's: `codex_app_server::register(Extension)` adds an extension to every
  thread (process-global: the TUI and exec start the app server in process), and
  `codex_tui::register(Account { login, logout, show })` puts an account behind /login,
  /auth, /logout and /status (without one, /login and /auth are not offered). hanzo-dev's
  `main` registers `hanzo_kai::install` and `hanzo_tui::account` before anything starts.
- `tui:`, `cli:`, `config:`, `core:`, `words:` the edits that were anchored strings:
  Dev's words and colors, `/model <name>`, the published `mcp_cmd`, `plugin_cmd` and
  `doctor` (they belong upstream as a pull request), `.hanzo` homes, the controller's
  calls in the loop, Hanzo's catalog and reviewers.
- `brand:` Dev's name in every user-facing string literal of non-test code.
- `version:` the version, below.

Dev reads only Hanzo's directories. Its home is `~/.hanzo/dev` (or `DEV_HOME`), a project's
settings live in `<repo>/.hanzo/dev/`, and system ones in `/etc/hanzo/dev/`; a `.codex` beside
them belongs to another product and is not read. Its server runs inside the process: upstream's
shared background server is installed from a package directory Dev does not ship
(`hanzo::BACKGROUND_SERVER`). A profile that names no provider is set to Hanzo at startup.
`crates/hanzo-dev/tests/launch.rs` launches a bare `dev` in a terminal and holds all three.

The version is the release's, set by hand in the root `Cargo.toml`'s `[workspace.package]`,
in `npm/package.json` and in the tag, and `release.yml` refuses a tag the two manifests do
not both say. hanzo-dev alone takes it: its `main` hands its own `CARGO_PKG_VERSION` to
`hanzo::version::set`, and every upstream crate asks `hanzo::version::get()` at run time.
Dev's libraries stay `0.0.0`. So a release recompiles hanzo-dev and the link, nothing
beneath.
`crates/hanzo-dev/tests/cli.rs` and `launch.rs` hold `--version`, the doctor and the TUI
header to the version the binary was built as, and fail on any upstream read of its own.

## Never edit upstream by hand

A change upstream hard-codes is a commit on hanzoai/codex `main`, made in `upstream/codex`
and pushed there before the gitlink here moves; never a build-time rewrite, never an
uncommitted edit in the checkout. Anything that need not live inside upstream's crates
goes in a `hanzo-*` crate here.

## The Code fork

just-every/code is reference, never a build input (its crates carry their own codex-core); its
clone lives at `~/work/just-every/code`, and what was ported from it is recorded in `migration/`.

## Working here

    make build          the dev binary
    make test           the Hanzo suite
    make test-upstream  hanzoai/codex's own suite against the pinned checkout
    make bump           how to move upstream/codex forward
    make help           everything

`make build` is the check that must pass. Fix warnings as well as errors.

## The release cache

Each release build runs rustc through sccache, and hanzoai/ci's `bin/cache` (pinned
by commit as `CI_SHA` in `release.yml`) carries the objects, the registry's crate
files and the git dependencies to the next run through the org's `ci-cache` bucket
in cloud's `/v1/s3`. The IAM token comes from the KMS client pair inside the restore
and save steps and is never exported, so no build script sees it.

- Name `<sha16 of rustc -vV + sccache --version>-<target>`; key `<sha32 of git ls-tree of
  Cargo.toml, Cargo.lock, rust-toolchain.toml, crates/ and the upstream/codex pin, and the
  RUSTFLAGS/CARGO_*/ZIG*/LLVM_*/XWIN_*/MACOS_SDK_* env>`. The release's version is in the
  root manifest and the lock, so each release misses its key and restores `latest` for its
  name, whose objects serve every crate but hanzo-dev.
- Only main and tags write, and only a key the store does not hold. Before a save the
  directory is cut to what that build touched: sccache bumps an object's mtime on a
  hit, and a crate file stays only if cargo unpacked it.
- What compiles every release anyway: proc-macro and build-script crates, which sccache
  cannot cache, the crates the commit changed and those above them, hanzo-dev (it carries
  the version) and the link. Before the version became a run-time value the stamp
  changed every first-party crate's version, which is in both `-C metadata` and
  `CARGO_PKG_VERSION`, so all ~150 recompiled on every release; that is why Dev's
  libraries stay `0.0.0`.
- Measured on dgx (its own target, two jobs, a busy host): cold, 1,046 crates in 59.6
  minutes; the same release from an empty target dir with a warm cache, 6.7 minutes and
  not one Rust miss (1,109 hits), hanzo-dev's own compile and the link ~4 of them; a
  new version alone rebuilds hanzo-dev and nothing else (measured then with the version
  passed as `DEV_VERSION`, which reached the same one crate).
- `sccache --show-stats` prints at the end of every Build step, pass or fail.

## Review and memory models

Automatic approval review and memory extraction and consolidation name a model
of their own. Upstream's ids are OpenAI's; api.hanzo.ai serves none of them. On
the Hanzo provider (`hanzo::is_hanzo`, base URL `hanzo::API_BASE`) all three are
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
- **Nothing waits that need not.** The first state's first call is begun the moment the state is
  ranked, beside the search (`flow::Ahead`, `zen::Zen::start`), against that state's revision. The
  search settling the task, or a kept step moving the state, cancels it; when the search settles
  nothing the call has had the search's time to answer, and the ledger's `generate` says how long
  (`ahead_ms`). A cancelled call closes its stream (the server stops writing it) and leaves a
  `cancelled` line in the spend file, its prompt counted at three characters a token at the ceiling
  rate; the ledger has a `cancel` line, never a `generate` one. An answer asked ahead is tried
  before any new call at its revision, and with one queued Kai is not asked `fix.turn@1`.
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
  repository, file or task. It solves single faults of that kind and nothing else. Past its 2.5 s
  cap it goes on while the call asked beside it has not answered, to 8 s (`mutate::LONGEST`): that
  wait is spent either way, and on a loaded host the cap cut 34 of 39 unsettled dev-split searches
  short of their candidates, the fix in the top four sites in 9 of them. A visible test too slow for
  the cap (its time x 1.5 > the cap) is narrowed to the failing tests it names (`mutate::screen`)
  instead of the search being skipped: lark's 18 s file runs its failing tests in 1.7 s, and the
  candidate picked is verified with the whole command in the tree as ever. A fast file narrowed
  ran 5% faster (two measurements, 13 and 19 tasks), not worth a second command there.
- **The tests around the failing ones** (`wide`) run beside the visible test after an edit, and a
  failing visible test stops them. What failed among them before any edit is read only when they
  fail after it, with the files the run touched put back as they were for that read and then as
  they are (`flow::before`, ledger `lazy`): a run that passes shows nothing new, so on most tasks
  it is never read, and it never competes with the search for the CPU.
- **A test run leaves nothing.** Files a run in the tree writes that git neither tracks nor ignores
  and that were not there before it are removed after it, with the directories they emptied
  (`tools::Litter`, ledger `sweep`): they are the run's, never the edit's. What `.gitignore` names
  is left alone, and so is a tree outside git. Hypothesis keeps its example database in the run's
  own temporary directory (`HYPOTHESIS_STORAGE_DIRECTORY`, unless the caller set it), so no run
  replays another's examples and none lands in the tree.
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

- **Model metadata**: a model the fork's catalog (`hanzo::catalog`) knows (`enso-auto` among them) gets its
  window and modalities from the catalog, so it raises no "Unknown model ... fallback
  metadata" warning (hanzoai/codex `core:` commit, models-manager).
- **Sandbox permissions**: a justification with no escalation asked for runs at the default
  permissions instead of failing the shell call (`core:` commit, core/src/tools/handlers).
- **ChatGPT legacy plugins 401**: featured plugins are asked for only with a ChatGPT sign-in
  (`auth.uses_codex_backend()`; `core:` commit, core-plugins).
- **Auth Symlink Guarantee**: `hanzo-config::ensure_parent_and_symlink` ensures `~/.hanzo/auth.json` is initialized with `{}` if absent before symlinking, preventing dangling `~/.hanzo/dev/auth.json`.
- **Token Refresh Command**: `DEFAULT_CONFIG` includes dynamic token refresh configuration `[model_providers.hanzo.auth] command = "hanzo auth token", refresh_interval_ms = 600000` to prevent 60-minute JWT expiration mid-session.

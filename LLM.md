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

`upstream/codex` and `upstream/code` are submodules. The gitlink is the pin —
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

`upstream/code` is reference, not a build input. Its crates depend on its own
fork of `codex-core`, so importing them would link two cores into one binary.
Its additions worth having — Auto Drive, the browser bridge — get rebuilt as
`hanzo-*` crates against `codex-core`. `migration/code-v1.patch` records what
v1 carried, including the defects it was already carrying; see
`migration/STATUS.md` before porting anything from it.

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

## Search

One search path: `crates/hanzo-search` (`hanzo_search::Search::query`) answers text
(regex), file names (glob) and symbols (word-boundary identifier, definitions first;
textual, with a `Resolver` hook for the LSP or code graph to confirm). The agent reaches
it as the native `grep` tool (`crates/hanzo-grep`), contributed through upstream's
extension API and installed by one anchored edit in the app server's
`thread_extensions`; the base instructions send the model to `grep`, and to `rg` in the
shell only when `grep` errors. `hanzo-search` is also a CLI that prints what the tool
returns (`-n` for `path:line:text`, `--files GLOB`, `--symbol`, `--json`, `--no-index`,
`--index-only`, `--prepare PATH`, `--stats`; exit 0/1/2 as rg).

- Engine: tgrep (MIT), the single search engine, linked as `tgrep-core`: a git dependency
  pinned by exact commit in `crates/hanzo-search/Cargo.toml`, upstream v1.2.1
  (`c7b2ab827274f8a56ff95eadc005f69e0732ee92`) until the hanzoai/tgrep fork's rev replaces
  it. Its rusqlite git pin would link a second SQLite beside sqlx's, so the root
  `Cargo.toml` patches it to rusqlite 0.39, which shares libsqlite3-sys 0.37.
- Per query path: the nearest `.tgrep/` index above it; a running tgrep server for that
  root (its `serve.json`, loopback JSON-RPC), else the on-disk index in process, else a
  scan with tgrep's walker (rg's ignore, hidden and binary rules). Nothing is spawned.
- Freshness: paths Dev names (`Search::touched`), files changed after a tool that may
  write started (`Search::since`, called by `hanzo-grep` on every other tool and command
  start), and, for an on-disk index, files whose size or mtime differs from its record
  are read from disk instead of the index; a row whose file is gone is dropped. The walk
  that finds them is the cost of an index that lags.
- Ranking: file score (text: matching lines; symbol: definitions x 2^32 + references;
  name: shallower first), then path, then line.
- Licence boundary: tgrep-core is MIT and linked; its notice is in `NOTICE`. ripgrep-all
  (`rga`, AGPL-3.0) is never used: not linked, copied, called, shipped, or compared.

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


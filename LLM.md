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

- Name `<sha16 of rustc -vV + sccache --version>-<target>`, key `<sha32 of Cargo.lock,
  rust-toolchain.toml, patches/*.json and the RUSTFLAGS/CARGO_*/ZIG*/LLVM_*/XWIN_*/
  MACOS_SDK_* env>`, both computed before the version stamp. A key that misses restores
  `latest` for that name.
- Only main and tags write, and only a key the store does not hold. Before a save the
  directory is cut to what that build touched: sccache bumps an object's mtime on a
  hit, and a crate file stays only if cargo unpacked it.
- What compiles every release anyway: every first-party crate (upstream's and ours),
  because the stamp changes their version, which is in both `-C metadata` and
  `CARGO_PKG_VERSION`; proc-macro and build-script crates, which sccache cannot
  cache; and the final link. On a quiet node the first-party chain from
  codex-protocol to the link is ~46 of a ~58-minute cold build, so that chain, not
  the dependencies, is what to shorten next.
- `sccache --show-stats` prints at the end of every Build step, pass or fail.

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


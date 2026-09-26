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

## Upstream

`upstream/codex` and `upstream/code` are submodules. The gitlink is the pin —
no lockfile of hashes sits beside it. Crates are consumed straight from the
checkout by ordinary path dependency.

`patches/upstream.json` carries the whole Hanzo delta: anchored substitutions that
`make prepare` applies to the checkout in place. `git -C upstream/codex status`
shows them, and that is the honest picture — the checkout is a build input we
own, not a pristine mirror. `make reset` returns it.

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

## Kai

Kai decides inside the loop; Zen only writes. Two crates:

- `crates/hanzo-loop` is what the loop asks a controller and applies: a tool shortlist per
  step, a command verdict joined with the policy on `allow < ask < deny` (`Handle::command`
  joins again, so no controller can loosen it), and routing hints for the turn's request
  metadata. It is small and stable on purpose: `codex-core` depends on it and rebuilds when
  it changes.
- `crates/hanzo-kai` links hanzoai/decision's `control` (Kai in process, batched) and
  `program` (the Decision Program executor), pinned by rev. `agent` is the controller per
  thread plus the extension contributors; `flow` runs `dev flow`; `trace` writes the decision
  lines; `tools` are the deterministic steps; `zen` is the only model call a flow makes.

The upstream delta is seven anchored edits in `patches/upstream.json`: `hanzo-loop` into
core's manifest; the shortlist asked in `built_tools` and applied in `build_tool_router`; the
verdict joined in the orchestrator right after the exec policy's requirement; the routing
hints set on the turn metadata in `run_turn`; `hanzo-kai` into app-server's manifest and
`hanzo_kai::install` before Guardian in `extensions.rs`.

`kai.toml` in the product home turns Kai on; without it nothing runs. Operations take Enso's
names (`tools`, `risk`, `model`, `reasoning`, `context`, `progress`, `complete`), each with a
`program`, a `mode` (default `shadow`), `thresholds` and `k`. Only `enforced` acts, and an
enforced decision is waited for at most 30 s. The trace (`kai/decisions.jsonl`) is Enso's
line shape, one line per decision, written once its outcome is known.

`dev flow fix --test "<cmd>"` runs `crates/hanzo-kai/flows/fix.json`: retrieval ranks the
repository's files against the failure, Kai picks one, Zen writes a diff, the tests run, and
Kai says done, retry or escalate within what the tests and the budget permit. A retried or
escalated attempt is undone. `--control rule` takes Kai out of the steering.

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

## House rules

- Hanzo service APIs use `https://api.hanzo.ai/v1/` exclusively.
- Keep the upstream interaction model and animation; the branding is ours.
- Keep Linux, macOS, and Windows working.
- Upstream project names appear only in `NOTICE` — never in the README, the
  repository description, the CLI, or the interface.

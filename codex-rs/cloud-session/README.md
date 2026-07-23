# codex-cloud-session

Streams a `hanzo code` run to the Hanzo Cloud session registry so it shows up
live in the hanzo.bot playground (`/sessions/:id`).

One implementation, shared by every frontend (`exec`, `tui`). It is decoupled
from any concrete `Config`: it authenticates through the `AuthManagerConfig`
trait and takes run details as a plain `SessionMeta` value.

## Contract

- `CloudSession::start(config, meta)` registers the run (`POST /v1/agents/sessions`)
  and returns `None` — silently — when tracking is opted out
  (`HANZO_SESSION_TRACKING=0`), the user is signed out, the base URL is untrusted,
  or the registry is unreachable. A failure to track never affects the run.
- `observe(&ServerNotification)` maps each event to the playground render
  contract (`message` · `tool-call` · `log` · `status` · `task` · `file_update`)
  and emits it on a detached task. `file_update` carries path + line counts only —
  never the diff body.
- `finish(status)` marks the run terminal (`done` | `error`).

Everything is additive and fire-and-forget: streaming is a side channel that can
never fail a run. The base URL (`CODEX_AGENTS_BASE_URL`, default
`https://api.hanzo.ai`) is trust-gated to HTTPS `*.hanzo.ai` or loopback, because
the signed-in bearer rides every request.

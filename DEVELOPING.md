# Development Notes

## Testing / Linting

- `./build-fast.sh`
- `pnpm install && pnpm lint`

## Auto Drive Fault Injection (development only)

Some retry/backoff paths are easier to exercise with injectable failures. Build
the TUI with the `dev-faults` feature and set the appropriate environment
variables before launching:

```bash
cd code-rs
cargo run --bin code-tui --features dev-faults
```

Available knobs (all optional):

- `CODEX_FAULTS_SCOPE`: currently only `auto_drive` is supported. Faults are
  ignored for other scopes.
- `CODEX_FAULTS`: comma-delimited list of `kind:count`. Supported kinds:
  - `disconnect` – simulate a transient “stream disconnected” error.
  - `429` – simulate a rate-limit response.
  Example: `CODEX_FAULTS="disconnect:2,429:1"` will fire two disconnects
  followed by one rate-limit.
- `CODEX_FAULTS_429_RESET`: optional reset hint for the 429 fault. Accepted
  values:
  - integer seconds (`120`)
  - `now+90s`
  - RFC3339 timestamp (`2025-09-29T12:00:00Z`)

When a fault fires, the CLI logs a `[faults] …` warning including the remaining
count. Without the feature flag (default builds), all hooks compile out and
production behaviour is unaffected.

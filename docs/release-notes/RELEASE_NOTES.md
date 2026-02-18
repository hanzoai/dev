## @hanzo/dev v0.6.74

Sync latest from openai/codex and just-every/code upstreams. All Hanzo branding, zen model presets, zap integration, and custom features preserved.

### Changes
- Upstream Sync: merge latest `openai/codex` main (citation fix, Windows sandbox, file watcher, token refresh, memory/rollout improvements)
- Upstream Sync: merge latest `just-every/code` main (v0.6.70, GPU landlock fix, search persistence, auto-drive routing)
- ZAP: split transport into module with HTTP/SSE support
- Core: refine zen model wording and cleanup comments
- Release: bump package/workspace versions to `0.6.74`

### Install
```
npm install -g @hanzo/dev@latest
code
```

## Overview
- AutoRunPhase now carries struct payloads; controller exposes helpers (`is_active`, `is_paused_manual`, `resume_after_submit`, `awaiting_coordinator_submit`, `awaiting_review`, `in_transient_recovery`).
- ChatWidget hot paths (manual pause, coordinator routing, ESC handling, review exit) rely on helpers/`matches!` instead of raw booleans.

## Tests
- `./build-fast.sh`

## Follow-ups
- See `docs/auto-drive-phase-migration-TODO.md` for remaining legacy-flag removals and snapshot coverage.

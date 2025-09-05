# Interrupt → Resume Postmortem

This document summarizes the root cause, symptoms, and fixes for the
"Esc then send never resumes" issue in the Code (Codex CLI, Rust) TUI/core
stack.

## Summary

Pressing Esc (or Ctrl+C) to interrupt a running turn, then immediately entering
a new message, frequently resulted in no new model turn starting. The UI showed
the user message, but there was no `TaskStarted`, and sometimes the system
appeared to hang until forcibly terminated.

Two independent problems contributed to this behavior:

1) A deadlock in `Session::abort()` in the core.
2) The TUI queuing policy after interrupts, which could strand messages.

## Symptoms

- After Esc, the next user message shows in history but no assistant output or
  `TaskStarted` appears.
- A second Ctrl+C exits the TUI, but the background task may linger.
- In earlier runs, messages sent while a task was running were queued waiting
  for `TaskComplete` (which never arrives after an `Interrupt`).

## Root Cause

### 1) Deadlock in `Session::abort()`

`Session::abort()` held the session mutex (`self.state.lock()`) and then called
`agent.abort()`. The `AgentAgent::abort()` emitted a protocol event using
`Session::make_event()`, which also attempted to lock `self.state`. This
re-entrant lock acquisition resulted in a deadlock:

- `Session::abort()` acquires the lock.
- Calls `agent.abort()` while still holding the lock.
- `agent.abort()` → `make_event()` tries to acquire the same lock → blocks
  forever.

Result: the submission loop remained stuck in the interrupt path and never got
to process the next `UserInput` turn.

### 2) Stranded messages after interrupts

Historically the TUI queued new user messages while a task was running, and
dispatched them on `TaskComplete`. After an `Interrupt`, there is no
`TaskComplete`, so the queued message could be left stranded indefinitely.

## Fixes

1) Remove the deadlock in `Session::abort()`:
   - Take (`take()`) the `current_agent` while holding the mutex, then drop the
     lock before calling `agent.abort()`. This ensures `make_event()` can safely
     acquire the lock again and avoids the self-deadlock.

2) Stronger atomicity around new input after interrupts:
   - For `Op::UserInput`, abort synchronously first (to ensure the prior agent
     is gone), then spawn and set the new agent. This prevents races where an
     async abort could kill the newly spawned agent.

3) TUI sending policy:
   - Always send user messages immediately, even if a task appears to be
     running. The core’s `UserInput` path aborts the prior agent and starts a
     fresh turn, so messages are never stranded waiting for a `TaskComplete`.

4) UX guards (non-functional):
   - Ignore late deltas after an interrupt so partial output does not trickle in
     post-cancel.
   - Drop late `ExecEnd` events for commands that were already marked as
     cancelled to avoid duplicate cells.

## Lessons Learned

- Never call back into code that may re-acquire a mutex you currently hold.
  Move side-effects (like emitting events) outside the lock’s critical section.
- Prefer a simple, predictable policy after interrupts: abort first, then spawn
  the next turn. Avoid queuing by frontends unless they can prove delivery.
- When diagnosing event-ordered systems, add short-lived diagnostics at the
  critical edges (submit, loop recv, abort begin/end, agent spawn, run begin)
  and then remove them once the root cause is fixed.

## Files/Areas Changed

- `core/src/codex.rs`
  - `Session::abort()` releases the lock before calling `agent.abort()`.
  - `Op::UserInput` aborts synchronously before spawning the next agent.

- `tui/src/chatwidget.rs`
  - Always send user input immediately (no queuing after interrupts).
  - Ignore late deltas after cancel; drop late ExecEnd for cancelled calls.

These changes resolved the “Esc then send never resumes” issue and restored a
reliable cancel→resume experience.


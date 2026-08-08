# TUI alternate screen and terminal multiplexers

## The conflict

The TUI draws into the terminal's **alternate screen buffer**, which gives it the
whole viewport and leaves your scrollback untouched when it exits — the same
thing vim and less do.

The xterm specification says an alternate screen buffer has no scrollback, and
[Zellij](https://github.com/zellij-org/zellij/pull/1032) follows that strictly
and offers no setting to relax it. So inside Zellij the terminal's own scroll
keys do nothing while the TUI is in alternate screen: the scrollback the
multiplexer would show simply does not exist. This is the specification working
as written, not a bug in either program. tmux and screen are laxer and usually
scroll anyway.

## What Hanzo Dev does about it

**Ctrl+T** toggles between the two buffers at any time.

Leaving the alternate screen replays the entire transcript into the normal
buffer, so the conversation lands in real terminal scrollback and your
multiplexer can page through it. Returning repaints the fullscreen view.

The choice is remembered: each toggle writes `tui.alternate_screen` to
`config.toml`, and that value decides which buffer the next session starts in.
Set it directly to pick a default:

```toml
[tui]
alternate_screen = false   # start in the normal buffer
```

## Where this lives

| What | Where |
|---|---|
| Toggle behavior | `code-rs/tui/src/app/terminal.rs` `toggle_screen_mode()` |
| Buffer switching | `code-rs/tui/src/tui.rs` `enter_alt_screen_only()` / `leave_alt_screen_only()` |
| Startup buffer | `code-rs/tui/src/app/init.rs` |
| Persisting the choice | `code_core::config::set_tui_alternate_screen()` |

## If a crash leaves the terminal wrong

```
reset
```

## Background

The scrollback problem was first reported against the upstream project in
[openai/codex#2558](https://github.com/openai/codex/issues/2558).

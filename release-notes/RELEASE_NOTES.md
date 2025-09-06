## @just-every/code v0.2.70

This release adds a time-based greeting in the TUI, improves Windows typing reliability, and fixes login and jump-back stability. It also enables connecting to a host Chrome from dev containers.

### Changes
- TUI: add time-based greeting placeholder across composer, welcome, and history; map 10–13 to "today".
- TUI/Windows: prevent double character echo by ignoring Release events without enhancement flags.
- Login: fallback to /oauth2/token and send Accept for reliable token exchange.
- TUI: fully reset UI after jump-back to avoid stalls when sending next message.
- TUI/Chrome: allow specifying host for external Chrome connection (dev containers).

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.69...v0.2.70

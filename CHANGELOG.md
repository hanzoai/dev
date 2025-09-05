Changelog

All notable changes to Code are documented here.

## [0.2.56] - 2025-09-01

- Strict event ordering in TUI: keep exec/tool cells ahead of the final assistant cell; render tool results from embedded markdown; stabilize interrupt processing. (dfb703a)
- Reasoning titles: better collapsed-title extraction and formatting rules; remove brittle phrase checks. (5ca1670, 7f4c569, 6d029d5)
- Plan streaming: queue PlanUpdate history while streaming to prevent interleaving; flush on finalize. (770d72c)
- De-dup reasoning: ignore duplicate final Reasoning events and guard out-of-order deltas. (f1098ad)

## [0.2.55] - 2025-09-01

- Reasoning delta ordering: key by `(item_id, output_index, content_index)`, record `sequence_number`, and drop duplicates/out-of-order fragments. (b39ed09, 509fc87)
- Merge streamed + final reasoning so text is not lost on finalize. (2e5f4f8)
- Terminal color detection: unify truecolor checks; avoid 256-color fallback on Windows Terminal; smoother shimmer gradients. (90fdb6a)
- Startup rendering: skip full-screen background paint on Windows Terminal; gate macOS Terminal behavior behind `TERM_PROGRAM` and `CODE_FORCE_FULL_BG_PAINT`. (6d7bc98)

## [0.2.54] - 2025-09-01

- Clipboard image paste: show `[image: filename]` placeholders; accept raw base64 and data-URI images; enable PNG encoding; add paste shortcut fallback to read raw images. (d597f0e, 6f068d8, d4287d2, 7c32e8e)
- Exec event ordering: ensure `ExecCommandBegin` is handled before flushing queued interrupts to avoid out-of-order “End” lines. (74427d4)
- ANSI color mapping: fix 256-indexed → RGB conversion and luminance decisions. (ddf6b68)

## [0.2.53] - 2025-09-01

- Browser + HUD: add CDP console log capture, collapsible HUD, and coalesced redraws; raise expanded HUD minimum height to 25 rows. (34f68b0, 1fa906d, d6fd6e5, 95ba819)
- General: improve internal browser launch diagnostics and log path. (95ba819)

## [0.2.52] - 2025-08-30

- Diff rendering: sanitize diff content like input/output (expand tabs, strip control sequences) to avoid layout issues. (7985c70)

## [0.2.51] - 2025-08-30

- CLI: de-duplicate `validateBinary` to avoid ESM redeclare errors under Bun/Node 23. (703e080)

## [0.2.50] - 2025-08-30

- CLI bootstrap: make bootstrap helper async and correctly await in the entry; fixes Bun global installs when postinstall is blocked. (9b9e50c)

## [0.2.49] - 2025-08-30

- CLI install: bootstrap the native binary on first run when postinstall is blocked; prefer cached/platform pkg then fall back to GitHub release. (27a0b4e)
- Packaging: adjust Windows optional dependency metadata for parity with published packages. (030e9ae)

## [0.2.48] - 2025-08-30

- TUI Help: show environment summary and resolved tool paths in the Help panel. (01b4a8c)
- CLI install safety: stop publishing a `code` bin by default; create a wrapper only when no PATH collision exists and remove on collision to avoid overriding VS Code. (1a95e83)

## [0.2.47] - 2025-08-30

- Agents: add `/agents` command; smoother TUI animations and safe branch names. (0b49a37)
- Core git UX: avoid false branch-change detection by ignoring quoted text and tokenizing git subcommands; show suggested confirm argv when blocking branch change. (7111b30, a061dc8)
- Exec cells: clearer visual status — black ❯ on completed commands, tinting for completed lines, and concise tree guides. (f2d31bb)
- Syntax highlighting: derive syntect theme from the active UI theme for cohesive code styling. (b8c06b5)

## [0.2.46] - 2025-08-30

- CLI postinstall: print clear guidance when a PATH collision with VS Code’s `code` is detected; suggest using `coder`. (09ebae9)
- Maintenance: upstream sync prior to release. (d2234fb)

## [0.2.45] - 2025-08-30

- TUI “glitch” animation: compute render rect first, scale safely, and cap height; bail early on tiny areas. (8268dd1)
- Upstream integration: adopt MCP unbounded channels and Windows target updates while keeping forked TUI hooks. (70bd689, 3b062ea)
- CI/infra: various stability fixes (Windows cache priming; clippy profile; unbounded channel). (7eee69d, 5d2d300, 970e466, 3f81840)

## [0.2.44] - 2025-08-29

- Exec UX: show suggested confirm argv when branch-change is blocked. (a061dc8)
- File completion: prioritize CWD matches for more relevant suggestions. (7d4cf9b)
- Assistant code cards: unify streaming/final layout; refine padding and colors; apply consistent background for code blocks. (e12f31c, 986a764, e4601bd, 97a91e8, beaa1c7)
- Syntax highlighting: theme-aware syntect mapping for better readability. (b8c06b5)

## [0.2.43] - 2025-08-29

- npx/bin behavior: always run bundled binary and show exact path; stop delegating to system VS Code. (448b176)
- Postinstall safety: remove global `code` shim if any conflicting `code` is on PATH; keep `coder` as the entrypoint. (1dc19da)
- Exec cells: clearer completed-state visuals and line tinting. (f2d31bb)

## [0.2.42] - 2025-08-29

- Housekeeping: release and sync tasks for CLI, core, and TUI. (eea7d98, 6d80b3a)

## [0.2.41] - 2025-08-29

- Housekeeping: release and pre-sync commits ahead of broader upstream merges. (75bb264, 75ed347)

## [0.2.40] - 2025-08-29

- Upstream sync: align web_search events and TUI popup APIs; clean warnings; maintain forked behaviors. (f20bffe, 4d9874f)
- Features: custom `/prompts`; deadlock fix in message routing. (b8e8454, f7cb2f8)
- Docs: clarify merge-only push policy. (7c7b63e)

## [0.2.39] - 2025-08-29

- Upstream integration: reconcile core/TUI APIs; add pager overlay stubs; keep transcript app specifics; ensure clean build. (c90d140, b1b01d0)
- Tools: add “View Image” tool; improve cursor after suspend; fix doubled lines/hanging markers. (4e9ad23, 3e30980, 488a402)
- UX: welcome message polish, issue templates, slash command restrictions while running. (bbcfd63, c3a8b96, e5611aa)

## [0.2.38] - 2025-08-29

- TUI: code-block background styling and improved syntax highlighting. (bb29c30)
- Markdown: strip OSC 8 hyperlinks; refine rendering and syntax handling. (a30c019)
- Exec rendering: highlight executed commands as bash and show inline durations. (38dc45a)
- Maintenance: merged fixes from feature branches (`feat/codeblock-bg`, `fix/strip-osc8-in-markdown`). (6b30005, 0704775)

## [0.2.37] - 2025-08-27

- Packaging: move platform-specific binaries to npm optionalDependencies; postinstall resolves platform package before GitHub fallback. (5bb9d01)
- CI: fix env guard for NPM_TOKEN and YAML generation for platform package metadata. (7ae25a9, d29be0a)

## [0.2.36] - 2025-08-27

- Packaging: switch CI to produce a single `code` binary and generate `code-tui`/`code-exec` wrappers. (7cd2b18)
- CI: stabilize cargo fetch and Windows setup; adjust --frozen/--locked usage to keep builds reliable. (5c6bf9f, 5769cec, 7ebcd9f)

## [0.2.35] - 2025-08-27

- Release artifacts: slimmer assets with dual-format (.zst preferred, .tar.gz fallback) and stripped debuginfo; smaller npm package. (f5f2fd0)

## [0.2.34] - 2025-08-26

- Clipboard: add raw image paste support and upstream TUI integration; fix Windows path separators and ESC/Ctrl+C flow. (0c6f35c, 0996314, 568d6f8, e5283b6)
- UX polish: reduce bottom padding, improve rate-limit message, queue messages, fix italic styling for queued. (d085f73, ab9250e, 251c4c2, b107918)
- Stability: token refresh fix; avoid showing timeouts as “sandbox error”. (d63e44a, 17e5077)

## [0.2.33] - 2025-08-26

- Maintenance: housekeeping after successful build; release tag. (2c6bb4d)

## [0.2.32] - 2025-08-25

- Sessions: fast /resume picker with themed table and replay improvements. (0488753)
- Input UX: double‑Esc behavior and deterministic MCP tool ordering; fix build warnings. (b048248, ee2ccb5, fcf7435)
- Core/TUI: per-session ExecSessionManager; ToolsConfig fixes with `new_from_params`. (15af899, 7b20db9)

## [0.2.31] - 2025-08-25

- Diff wrapping: add one extra space to continuation hang indent for perfect alignment. (bee040a)

## [0.2.30] - 2025-08-24

- Diff summary: width-aware patch summary rendering with hanging indent; always show gutter icon at top of visible portion. (03beb32, 41b7273)

## [0.2.29] - 2025-08-24

- Version embedding: prefer `CODE_VERSION` env with fallback to Cargo pkg version across codex-rs; update banners and headers. (af3a8bc)

## [0.2.28] - 2025-08-24

- Windows toolchain: refactor vcpkg + lld-link config; ensure Rust binary embeds correct version in release. (9a57ec3, 8d61a2c)

## [0.2.27] - 2025-08-24

- Web search: integrate tool and TUI WebSearch event/handler; keep browser + agent tools; wire configs and tests. (6793a2a, a7c514a, 0994b78)
- CI: faster cross-platform linking/caching; streamlined Cargo version/lockfile updates. (c7c28f2, 5961330)

## [0.2.26] - 2025-08-24

- CI: improved caching and simplified release workflows for reliability. (e37a2f6, 8402d5a)

## [0.2.25] - 2025-08-24

- Release infra: multiple small workflow fixes (build version echo, Rust release process). (ac6b56c, 64dda2d)

## [0.2.24] - 2025-08-24

- Release workflow: update Rust build process for reliability. (64dda2d)

## [0.2.23] - 2025-08-24

- CI: fix build version echo in release workflow. (2f0bdd1)

## [0.2.22] - 2025-08-24

- Release workflow: incremental YAML fixes and cleanup. (3a88196, 7e4cea1)

## [0.2.21] - 2025-08-24

- CI cache: use `SCCACHE_GHA_VERSION` to restore sccache effectiveness. (43e4c05)

## [0.2.20] - 2025-08-24

- Docs: add module description to trigger CI and verify doc gating. (e4c4456)

## [0.2.19] - 2025-08-24

- CI: move sccache key configuration; tighten input responsiveness and diff readability in TUI. (46e57f0, 9bcf7c7)

## [0.2.18] - 2025-08-24

- TUI: clean unused `mut` and normalize overwrite sequences; preserve warning-free builds. (621f4f9)

## [0.2.17] - 2025-08-24

- TUI: housekeeping and stable sccache cache keys. (85089e1, 17bbc71)

## [0.2.16] - 2025-08-23

- Navigation: gate Up/Down history keys when history isn’t scrollable to avoid dual behavior. (150754a)

## [0.2.15] - 2025-08-23

- CI: stabilize sccache startup to fix slow releases. (f00ea33)

## [0.2.14] - 2025-08-23

- CI: small test to validate caching; no product changes. (7ebd744)

## [0.2.13] - 2025-08-23

- Build cleanliness: fix all warnings under build-fast. (0356a99)

## [0.2.12] - 2025-08-23

- CI: correct SCCACHE_DIR usage, export/guard env, and make caching resilient; better heredoc detection for apply_patch. (0a59600, b10c86a, c263b05, 39a3ec8, de54dbe)

## [0.2.11] - 2025-08-23

- Rendering: fully paint history region and margins to remove artifacts; add transcript hint and aggregated-output support. (b6ee050, ffd1120, eca97d8, 957d449)

## [0.2.10] - 2025-08-23

- Stability: align protocol/core with upstream; fix TUI E0423 and history clearing; regenerate Cargo.lock for locked builds. (52d29c5, 663d1ad, 2317707, da80a25)

## [0.2.9] - 2025-08-21

- Transcript mode: add transcript view; hide chain-of-thought by default; show “thinking” headers. (2ec5a28, e95cad1, 9193eb6)
- Exec ordering: insert running exec into history and replace in place on completion to prevent out-of-order rendering. (c1a50d7)
- Onboarding: split onboarding screen to its own app; improve login handling. (0d12380, c579ae4)

## [0.2.8] - 2025-08-21

- Exec previews: use middle-dot ellipsis and concise head/tail previews; rely on Block borders for visuals. (1ac3a67, 352ce75, 5ca0e06)

## [0.2.7] - 2025-08-20

- Browser tool: robust reconnect when cached Chrome WS URL is stale; clearer screenshot strategy and retries. (9516794)
- Merge hygiene and build fixes from upstream while keeping forked UX. (fb08c84, d79b51c)

## [0.2.6] - 2025-08-20

- History: live timers for custom/MCP tools; stdout preview for run commands; clearer background events. (f24446b, 5edbbe4, 2b9c1c9)
- Apply patch: auto-convert more shell-wrapped forms; suppress noisy screenshot-captured lines. (2fb30b7, 3da06e5)

## [0.2.5] - 2025-08-19

- CLI downloads: verify Content-Length, add timeouts/retries, and improve WSL guidance for missing/invalid binaries. (ca55c2e)

## [0.2.4] - 2025-08-19

- Windows CLI: guard against corrupt/empty downloads; clearer spawn error guidance (EFTYPE/ENOEXEC/EACCES). (bb21419)

## [0.2.3] - 2025-08-19

- Release CI: enable sccache and mold; tune incremental to improve cache hit rate. (69f6c3c)

## [0.2.2] - 2025-08-19

- Protocol alignment and dep bumps across codex-rs; login flow async-ified; smaller fixes. (4db0749, 6e8c055, 38b84ff)

## [0.2.1] - 2025-08-19

- Fork stabilization: large upstream sync while preserving TUI/theme and protocol; add tests and clean colors/styles. (b8548a0, 47ba653, c004ae5)

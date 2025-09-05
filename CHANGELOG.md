Changelog

All notable changes to this project are documented here. This file is reconstructed from GitHub releases and mapped commit summaries. Dates use UTC tag dates.

## [0.2.56] - 2025-09-01

- Strict event ordering in TUI: keep exec/tool cells ahead of the final assistant cell; render tool results from embedded markdown; stabilize interrupt processing. (dfb703a)
- Reasoning titles: better collapsed-title extraction and formatting rules; remove brittle phrase checks. (5ca1670, 7f4c569, 6d029d5)
- Plan streaming: queue PlanUpdate history while streaming to prevent interleaving; flush on finalize. (770d72c)
- De-dup reasoning: ignore duplicate final Reasoning events and guard out-of-order deltas. (f1098ad)

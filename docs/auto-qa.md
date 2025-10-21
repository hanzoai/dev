# Auto QA Orchestration

## Overview
- Auto Drive no longer hands QA control to the coordinator.
- A dedicated QA orchestrator thread watches turn completions and emits QA events.

## Coordinator Schema Changes
- `code_review` and `cross_check` fields were removed from `CoordinatorTurnNew` and are rejected if present.
- Coordinator decisions now focus on CLI + agent planning only.

## QA Orchestrator Responsibilities
- Emit `AppEvent::AutoQaUpdate { note }` every cadence window (default 3 turns).
- Emit `AppEvent::AutoReviewRequest { summary }` when a diff-bearing turn satisfies the review cooldown. The chat widget no longer launches review on every write turn; these requests are the sole entry point for automated code review.
- Cadence and cooldown state resets when the orchestrator shuts down.

## Coordinator Finalization
- When the coordinator produces `finish_success`, Auto Drive triggers a forced cross-check run before completing.
- A successful cross-check forwards the stored success decision to the UI; any failing cross-check converts the result into a coordinator failure and surfaces the restart banner so automation can recover instead of exiting early.

## Start / Stop Conditions
- ChatWidget starts the orchestrator when Auto Drive launches and at least one QA feature (review, cross-check, observer) is enabled.
- The orchestrator shuts down whenever Auto Drive stops.
- Review requests still respect `review_enabled`; handlers reuse existing post-turn review helpers.

## Environment Knobs
- `CODE_QA_CADENCE` &mdash; turn cadence between `AutoQaUpdate` notes (default `3`).
- `CODE_QA_REVIEW_COOLDOWN_TURNS` &mdash; turns with diffs before an automated `AutoReviewRequest` is sent (default `1`).

## Future Work
- Consolidate QA flags behind a single `qa_automation_enabled` toggle in `AutoDriveSettings`.

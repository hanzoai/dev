import { OrchestrationV2DomainEvent } from "@hanzodev/contracts";
import * as Effect from "effect/Effect";
import { readFileSync } from "node:fs";
import * as Schema from "effect/Schema";
import { expect } from "vitest";

import type { OrchestratorV2ScenarioResult } from "../OrchestratorScenario.ts";
import type {
  OrchestratorFixtureInput,
  ProviderOrchestratorReplayVariant,
} from "./fixtures/shared.ts";

/**
 * Language-neutral provider fixtures: `crates/provider/fixtures/<fixture>/<driver>.*`.
 * `transcript.ndjson` is the recorded provider wire, `input.json` the steps that drive it,
 * `events.json` the domain events a replay must produce. TS and Rust replay the same files.
 */
const FIXTURES_ROOT = new URL("../../../../../crates/provider/fixtures/", import.meta.url);

const encodeEvents = Schema.encodeSync(Schema.Array(OrchestrationV2DomainEvent));

/**
 * Replays whose event order or set differs from run to run, because tasks race in the TS
 * adapter. Their transcript and input are pinned; their events are not, until the Rust
 * adapter decides the order.
 */
const RACY_EVENTS = new Set([
  "grok_auto_blocked_command/grok",
  "kiro_tool_call_read_only_on_request/kiro",
  "queued_cancelled_while_active/codex",
  "subagent/kiro",
  "acp_titled_multiselect/acpRegistry",
  "claude_background_subagent_lifecycle/claudeAgent",
  "claude_background_wake_before_queued_prompt/claudeAgent",
  "claude_background_wake_before_queued_prompt_no_echo/claudeAgent",
  "thread_rollback_after_stop/pi",
  "tool_call_read_only_on_request/grok",
]);

const UUID = /[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/g;
const TIMESTAMP = /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z/g;

/**
 * Removes what differs between runs, as `manifest.json` states it: the temp workspace
 * path, timestamps (wall-clock and test-clock alike; a race shifts the test clock by a millisecond), and UUIDs the adapter minted,
 * numbered by first appearance. UUIDs that occur in the transcript are the provider's
 * and stay as recorded.
 */
function normalizeEvents(
  text: string,
  input: { readonly workspace: string; readonly transcript: string },
): string {
  const recorded = new Set(input.transcript.match(UUID) ?? []);
  const minted = new Map<string, string>();
  return [`/private${input.workspace}`, input.workspace]
    .reduce((out, workspace) => out.replaceAll(workspace, "<workspace>"), text)
    .replace(TIMESTAMP, "<time>")
    .replace(UUID, (id) => {
      if (recorded.has(id)) return id;
      const existing = minted.get(id);
      if (existing !== undefined) return existing;
      const next = `<uuid:${minted.size}>`;
      minted.set(id, next);
      return next;
    });
}

function json(value: unknown): string {
  return `${JSON.stringify(value, null, 2)}\n`;
}

/**
 * Pins one replay against the shared fixture files. A missing or stale file fails the run;
 * `vp test run -u` rewrites it, so a changed adapter shows up as a reviewable diff.
 */
export const matchProviderFixture = Effect.fn("matchProviderFixture")(function* (input: {
  readonly fixtureName: string;
  readonly driver: ProviderOrchestratorReplayVariant;
  readonly fixtureInput: OrchestratorFixtureInput;
  readonly result: OrchestratorV2ScenarioResult;
  /** The replay workspace; its random temp path is recorded as `<workspace>`. */
  readonly workspace: string;
}) {
  const driver = input.driver.driver;
  const file = (name: string) =>
    new URL(`${input.fixtureName}/${driver}.${name}`, FIXTURES_ROOT).pathname;
  const transcript = readFileSync(new URL(input.driver.transcriptFile), "utf8");
  const inputJson = json({
    fixture: input.fixtureName,
    driver,
    modelSelection: input.driver.modelSelection,
    runContinuationWorker: input.driver.runContinuationWorker === true,
    interactionMode: input.fixtureInput.interactionMode ?? null,
    runtimeMode: input.fixtureInput.runtimeMode ?? null,
    workspaceFiles: input.fixtureInput.workspaceFiles ?? {},
    steps: input.fixtureInput.steps,
  });
  const eventsJson = normalizeEvents(json(encodeEvents(input.result.domainEvents)), {
    workspace: input.workspace,
    transcript,
  });
  yield* Effect.promise(async () => {
    await expect(transcript).toMatchFileSnapshot(file("transcript.ndjson"));
    await expect(inputJson).toMatchFileSnapshot(file("input.json"));
    if (!RACY_EVENTS.has(`${input.fixtureName}/${driver}`)) {
      await expect(eventsJson).toMatchFileSnapshot(file("events.json"));
    }
  });
});

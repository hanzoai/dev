import { assert, describe, it } from "@effect/vitest";
import * as Tracer from "effect/Tracer";
import * as Effect from "effect/Effect";

import { materializeReplayTranscriptWorkspace } from "@hanzodev/provider-testing/replayTranscript";
import {
  HANZO_DRIVER_KIND,
  HANZO_GEN_DRIVER_KIND,
  defaultInstanceIdForDriver,
} from "@hanzodev/contracts";
import {
  CodexOrchestratorReplayHarness,
  makeCodexOrchestratorReplayHarness,
} from "../Adapters/CodexAdapterV2.testkit.ts";
import {
  findReplayFixture,
  itRunsReplayFixtures,
  runReplayFixture,
} from "./ProviderReplayFixtures.ts";

describe("Codex replay fixtures", () => {
  itRunsReplayFixtures("codex", (input) =>
    runReplayFixture({
      ...input,
      harness: CodexOrchestratorReplayHarness,
      materializeWorkspace: (transcript, workspace) =>
        Effect.succeed(materializeReplayTranscriptWorkspace(transcript, workspace)),
    }),
  );
});

it.effect(
  "persists recorded Codex snapshots through provider transactions in the real run consumer",
  () =>
    Effect.gen(function* () {
      const fixture = findReplayFixture("multi_turn")!;
      const driver = fixture.providers.find((provider) => provider.driver === "codex")!;
      const writes: number[] = [];
      const tracer = Tracer.make({
        span(options) {
          const span = new Tracer.NativeSpan(options);
          const end = span.end.bind(span);
          span.end = (time, exit) => {
            end(time, exit);
            if (span.name === "orchestrationV2.EventSink.providerBatch") {
              writes.push(Number(span.attributes.get("orchestration_v2.batch_writes")));
            }
          };
          return span;
        },
      });
      yield* runReplayFixture({
        fixtureName: fixture.name,
        buildInput: fixture.buildInput,
        driver,
        harness: CodexOrchestratorReplayHarness,
        materializeWorkspace: (transcript, workspace) =>
          Effect.succeed(materializeReplayTranscriptWorkspace(transcript, workspace)),
      }).pipe(Effect.withTracer(tracer), Effect.withTracerEnabled(true));
      assert.equal(
        writes.reduce((total, count) => total + count, 0),
        6,
      );
      assert.isTrue(writes.every((count) => count <= 3));
    }),
);

/** Every value stored under a `driver` key anywhere in `value`. */
function driverFields(value: unknown, seen = new Set<unknown>()): ReadonlyArray<unknown> {
  if (value === null || typeof value !== "object" || seen.has(value)) return [];
  seen.add(value);
  if (value instanceof Map) return driverFields([...value.values()], seen);
  if (Array.isArray(value)) return value.flatMap((entry) => driverFields(entry, seen));
  return Object.entries(value).flatMap(([key, nested]) => [
    ...(key === "driver" ? [nested] : []),
    ...driverFields(nested, seen),
  ]);
}

// regression: #5 Hanzo and Hanzo Gen wrap the Codex adapter, and their sessions, threads and
// items reported driver codex.
describe("Hanzo drivers on the Codex adapter", () => {
  it.effect.each([HANZO_DRIVER_KIND, HANZO_GEN_DRIVER_KIND])(
    "%s replays report only their own driver",
    (driverKind) =>
      Effect.gen(function* () {
        const fixture = findReplayFixture("multi_turn")!;
        const codex = fixture.providers.find((provider) => provider.driver === "codex")!;
        const result = yield* runReplayFixture({
          fixtureName: fixture.name,
          buildInput: fixture.buildInput,
          driver: {
            ...codex,
            driver: driverKind,
            modelSelection: {
              ...codex.modelSelection,
              instanceId: defaultInstanceIdForDriver(driverKind),
            },
            // The fixture's checks expect the transcript to name the replayed provider instance.
            assertOutput: (output, transcript) =>
              codex.assertOutput(output, { ...transcript, provider: driverKind }),
          },
          harness: makeCodexOrchestratorReplayHarness(driverKind),
          materializeWorkspace: (transcript, workspace) =>
            Effect.succeed(materializeReplayTranscriptWorkspace(transcript, workspace)),
        });
        const drivers = driverFields([
          result.storedEvents,
          result.domainEvents,
          result.projections,
          result.shellSnapshot,
        ]);
        assert.isNotEmpty(drivers);
        assert.deepEqual([...new Set(drivers)], [driverKind]);
      }),
  );
});

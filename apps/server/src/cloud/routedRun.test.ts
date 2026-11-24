import { assert, it } from "@effect/vitest";
import {
  RunId,
  ThreadId,
  type OrchestrationV2Command,
  type OrchestrationV2DomainEvent,
  type OrchestrationV2RunStatus,
  type OrchestrationV2StoredEvent,
} from "@hanzodev/contracts";
import * as Effect from "effect/Effect";

import {
  commandProviderTarget,
  createRoutedRunLeaseRegistry,
  isRoutedSession,
  isRunStartingCommand,
} from "./routedRun.ts";

const command = (fields: Record<string, unknown>) => fields as unknown as OrchestrationV2Command;

it("only a Hanzo Link session proving a DPoP key is a routed session", () => {
  assert.equal(isRoutedSession({ method: "dpop-access-token", subject: "cloud-connect" }), true);
  // A device paired by code is the owner's own and is not routed.
  assert.equal(isRoutedSession({ method: "dpop-access-token", subject: "one-time-token" }), false);
  assert.equal(
    isRoutedSession({ method: "browser-session-cookie", subject: "cloud-connect" }),
    false,
  );
  assert.equal(isRoutedSession({ method: "bearer-access-token", subject: "cloud-connect" }), false);
});

it("names the provider instance a run-starting command would use", () => {
  const modelSelection = { instanceId: "claude" };
  assert.deepEqual(commandProviderTarget(command({ type: "thread.create", modelSelection })), {
    _tag: "instance",
    instanceId: "claude",
  });
  assert.deepEqual(commandProviderTarget(command({ type: "provider.switch", modelSelection })), {
    _tag: "instance",
    instanceId: "claude",
  });
  assert.deepEqual(commandProviderTarget(command({ type: "message.dispatch", threadId: "t1" })), {
    _tag: "thread",
    threadId: ThreadId.make("t1"),
  });
  assert.deepEqual(
    commandProviderTarget(command({ type: "message.dispatch", threadId: "t1", modelSelection })),
    { _tag: "instance", instanceId: "claude" },
  );
  assert.equal(commandProviderTarget(command({ type: "thread.archive" })), null);
});

const runEvent = (
  type: "run.created" | "run.updated",
  threadId: string,
  runId: string,
  status: OrchestrationV2RunStatus,
): OrchestrationV2DomainEvent =>
  ({
    type,
    threadId: ThreadId.make(threadId),
    payload: { id: RunId.make(runId), threadId: ThreadId.make(threadId), status },
  }) as unknown as OrchestrationV2DomainEvent;

const stored = (event: OrchestrationV2DomainEvent): OrchestrationV2StoredEvent =>
  ({ event }) as unknown as OrchestrationV2StoredEvent;

it.effect("enforces one active routed run per login instance and releases when the run ends", () =>
  Effect.gen(function* () {
    const leases = createRoutedRunLeaseRegistry();
    assert.equal(isRunStartingCommand(command({ type: "message.dispatch" })), true);
    assert.equal(isRunStartingCommand(command({ type: "delegated_task.request" })), true);
    assert.equal(isRunStartingCommand(command({ type: "thread.create" })), false);

    // First routed run acquires and binds to run-1 on claude.
    yield* leases.tryAcquire("claude", "cmd-1");
    leases.bindEvents("claude", "cmd-1", [
      stored(runEvent("run.created", "t1", "run-1", "running")),
    ]);

    // A second routed run on the same instance is refused while run-1 is active.
    const second = yield* Effect.exit(leases.tryAcquire("claude", "cmd-2"));
    assert.equal(second._tag, "Failure");

    // A different instance is unaffected and can run concurrently.
    yield* leases.tryAcquire("codex", "cmd-codex");
    leases.bindEvents("codex", "cmd-codex", [
      stored(runEvent("run.created", "t2", "run-2", "running")),
    ]);

    // When run-1 completes via the domain event stream, claude is freed.
    leases.onDomainEvent(runEvent("run.updated", "t1", "run-1", "completed"));
    yield* leases.tryAcquire("claude", "cmd-3");
    assert.deepEqual(leases.activeLeaseFor("claude"), { kind: "starting", commandId: "cmd-3" });

    // A failed dispatch releases its starting reservation immediately.
    leases.releaseIfStarting("claude", "cmd-3");
    assert.equal(leases.activeLeaseFor("claude"), undefined);

    // Projection fallback clears a stale lease if an end event was missed.
    yield* leases.tryAcquire("codex", "cmd-codex-2", () => Effect.succeed(false));
    assert.deepEqual(leases.activeLeaseFor("codex"), {
      kind: "starting",
      commandId: "cmd-codex-2",
    });
  }),
);

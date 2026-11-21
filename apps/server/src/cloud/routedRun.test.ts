import { assert, it } from "@effect/vitest";
import { ThreadId, type OrchestrationV2Command } from "@hanzodev/contracts";

import { commandProviderTarget, isRoutedSession } from "./routedRun.ts";

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

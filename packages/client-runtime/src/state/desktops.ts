import { WS_METHODS, type EnvironmentId } from "@hanzodev/contracts";
import * as Effect from "effect/Effect";
import { Atom } from "effect/reactivity";
import type { AtomRegistry } from "effect/reactivity";

import type { EnvironmentRegistry } from "../connection/registry.ts";
import { createEnvironmentRpcCommand, createEnvironmentRpcQueryAtomFamily } from "./runtime.ts";

/** HIP-1350: this install's link to Hanzo, and the user's other linked desktops. */
export function createDesktopEnvironmentAtoms<R, E>(
  runtime: Atom.AtomRuntime<EnvironmentRegistry | R, E>,
) {
  const status = createEnvironmentRpcQueryAtomFamily(runtime, {
    label: "environment-data:desktops:status",
    tag: WS_METHODS.desktopsStatus,
    staleTimeMs: 5_000,
  });
  const list = createEnvironmentRpcQueryAtomFamily(runtime, {
    label: "environment-data:desktops:list",
    tag: WS_METHODS.desktopsList,
    // Listing asks Hanzo, so a list stays fresh briefly.
    staleTimeMs: 10_000,
  });
  const refresh = (
    { environmentId }: { readonly environmentId: EnvironmentId },
    registry: AtomRegistry.AtomRegistry,
  ) =>
    Effect.sync(() => {
      registry.refresh(status({ environmentId, input: {} }));
      registry.refresh(list({ environmentId, input: {} }));
    });
  return {
    status,
    list,
    link: createEnvironmentRpcCommand(runtime, {
      label: "environment-data:desktops:link",
      tag: WS_METHODS.desktopsLink,
      concurrency: { mode: "singleFlight", key: ({ environmentId }) => environmentId },
      onSuccess: refresh,
    }),
    unlink: createEnvironmentRpcCommand(runtime, {
      label: "environment-data:desktops:unlink",
      tag: WS_METHODS.desktopsUnlink,
      concurrency: { mode: "singleFlight", key: ({ environmentId }) => environmentId },
      onSuccess: refresh,
    }),
    rename: createEnvironmentRpcCommand(runtime, {
      label: "environment-data:desktops:rename",
      tag: WS_METHODS.desktopsRename,
      onSuccess: refresh,
    }),
    revoke: createEnvironmentRpcCommand(runtime, {
      label: "environment-data:desktops:revoke",
      tag: WS_METHODS.desktopsRevoke,
      onSuccess: refresh,
    }),
  };
}
